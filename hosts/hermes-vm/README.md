# Hermes VM

Microvm running [hermes-agent](https://hermes-agent.nousresearch.com/) — the homelab's personal assistant. Isolated from compute via its own kernel (qemu), read-only clone of the Obsidian vault from gitea, separate sops trust domain.

## Hardware

- **Hypervisor**: qemu, 2 vCPU, 4 GB RAM (`2048` triggers a known qemu hang — see `microvm.nix #171`)
- **Persistent volume**: 4 GB at `/var/lib/hermes` — Honcho memory, sessions, mcpvault npx cache, SSH host keys, vault clone
- **Share**: `/nix/store` (RO virtiofs)
- **Network**: bridge `compute-microvm` on compute, VM at `10.20.1.10/24`, NAT egress through compute's `bond0`

## Architecture

- **Self-contained secrets via sops-nix**. The VM decrypts `dotfiles-private/hosts/hermes-vm/secrets.yaml` using its ed25519 SSH host key as the age identity (sops-nix converts ssh-ed25519 → age automatically). One key handles both SSH login and sops decryption; the host key persists on `/var/lib/hermes` across rebuilds.
- **Late activation in stage 2**. The volume isn't mounted in initrd, so sops can't find the SSH host key until userspace. `sops-late-activate.service` re-runs the (idempotent) activation script after `local-fs.target`, gated by the host key's presence.
- **`config.yaml` is non-secret**, rendered inside the VM from its own evaluated settings and installed via `ExecStartPre` post-mount (the upstream module's activation-time write gets shadowed by the volume mount).
- **Human admin only**. `bphenriques` is the sole sudoer; the `hermes` service user has no shell access, no wheel, no SSH keys. SSH from laptop via ProxyJump compute: `ssh hermes-vm`.
- **Vault clone, not share**. `vault-sync.service` clones `bphenriques/notes` from gitea on boot (depth 1) using the `hermes-agent` gitea credentials, refreshed every 5 min by `vault-sync.timer`. Clone is owned `root:hermes` mode 0750 — the agent reads but can't mutate. Any local change gets blown away on the next sync.

For the broader design context (microvm isolation rationale), see [`bphenriques-tools/microvm.md`](../../bphenriques-tools/microvm.md).

## Setup

This VM bootstraps off its own SSH host key. The key is generated on first boot and lives on the persistent volume; capturing it once and adding it as a sops recipient is the whole "key management."

### 1. Initial deploy (no secrets yet)

In `dotfiles-private`, expose an **empty** secrets file:

```nix
# dotfiles-private/flake.nix output
hosts.hermes-vm.sopsSecretsFile = ./hosts/hermes-vm/secrets.yaml;
```

Comment out anything in `hosts/hermes-vm/default.nix` that reads from sops (or just `services.hermes-agent.enable = false;`) so the first boot can complete without decryption. Deploy:

```bash
nixos-rebuild switch --flake .#compute --target-host compute --use-remote-sudo
```

This brings up the VM, sshd generates its ed25519 host key at `/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key`.

### 2. Capture the host key as an age recipient

```bash
ssh hermes-vm 'sudo cat /var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key.pub' | nix run nixpkgs#ssh-to-age
```

Prints `age1...`. Add it to `dotfiles-private/.sops.yaml` as a recipient for `hosts/hermes-vm/secrets.yaml`.

### 3. Create the encrypted secrets

```bash
sops edit dotfiles-private/hosts/hermes-vm/secrets.yaml
```

```yaml
hermes-agent:
  api-server-key: <random 32 hex chars>
  gitea-token: <placeholder — overwritten in step 5>
```

`gitea-password` (the user's actual gitea password, used by gitea-configure on compute) lives in `hosts/compute/secrets.yaml`. The VM never sees it.

### 4. Re-deploy with secrets enabled

Re-enable whatever you disabled in step 1, then:

```bash
nixos-rebuild switch --flake .#compute --target-host compute --use-remote-sudo
ssh compute 'sudo systemctl restart microvm@hermes-vm'
```

On boot: VM mounts `/var/lib/hermes`, `sops-late-activate.service` fires once the host key path exists and runs the activation script, which decrypts secrets into `/run/secrets/`. `hermes-agent` starts and listens on `0.0.0.0:8642`. Traefik on compute proxies `hermes-api.{domain}` → VM.

### 5. Issue the gitea PAT for vault-sync

Gitea has `ENABLE_BASIC_AUTHENTICATION = false` so the agent can't clone via password — it needs a personal access token. The `gitea-admin` wrapper (provisioned on compute) hides the `sudo -u gitea gitea -c <config>` plumbing.

**Create:**
```bash
gitea-admin admin user generate-access-token \
  --username hermes-agent \
  --scopes 'read:repository' \
  --token-name vault-sync
```

Capture the printed token, edit the VM's sops file, replace the `gitea-token` placeholder. Redeploy compute (so the VM picks up the new secret), then `ssh hermes-vm 'sudo systemctl restart vault-sync hermes-agent'`.

**Rotate:** issue a new token with a different `--token-name` (e.g. `vault-sync-2`), swap the value in the VM's sops file, redeploy + restart vault-sync, then revoke the old token from the gitea UI (Settings → Applications → Access Tokens). The rotate-then-revoke order keeps vault-sync working through the swap.

**Revoke:** gitea UI → Settings → Applications → find the token by name → Delete. There's no `gitea admin` CLI subcommand for token deletion; it's UI/API only. The `--token-name vault-sync` you set at creation is how you find it.

### 6. Smoke test

```bash
# SSH into the VM (alias defined in home-manager):
ssh hermes-vm

# From inside, smoke trio:
hermes chat -q "What time is it in Lisbon?"
hermes chat -q "List the top-level folders in my notes vault."
hermes chat -q "Search my vault for car expenses."

# From the laptop, hit the API via Traefik:
curl -fsS -H "Authorization: Bearer <api-server-key>" https://hermes-api.<domain>/v1/models
```

The API token is the `hermes-agent/api-server-key` value from `dotfiles-private/hosts/hermes-vm/secrets.yaml`.

## Operational notes

- **VM stuck initialising**: check `journalctl -u hermes-agent -n 50 --no-pager` (`bphenriques` is in `systemd-journal`). Common causes: Ollama unreachable from VM, pip lazy-installs on first chat (~1 min), or the `2 GiB qemu hang` if `mem` was set to exactly 2048.
- **GPU offload regressed**: check `nvidia-persistenced` on the laptop. CUDA-detection can fail across suspends; restart `ollama.service` to re-probe.
- **Vault stale / empty**: `systemctl status vault-sync.service` and `journalctl -u vault-sync`. Auth failures show as `git: fatal: Authentication failed` — verify `hermes-agent` still has read access to `bphenriques/notes` in gitea.
- **SSH host key changed after VM rebuild**: shouldn't happen — host keys persist on the volume — but if it does, `ssh-keygen -R 10.20.1.10` and re-accept once.
