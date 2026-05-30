# personal-agent

Microvm running the homelab's personal assistant — currently [hermes-agent](https://hermes-agent.nousresearch.com/). Isolated from compute via its own kernel (qemu), read-only clone of the Obsidian vault from gitea, separate sops trust domain.

## Hardware

- **Hypervisor**: qemu, 2 vCPU, 4 GB RAM (`2048` triggers a known qemu hang — see `microvm.nix #171`)
- **Persistent volume**: 4 GB at `/var/lib/hermes` — Honcho memory, sessions, mcpvault npx cache, SSH host keys, vault clone
- **Share**: `/nix/store` (RO virtiofs)
- **Network**: bridge `compute-microvm` on compute, VM at `10.20.1.10/24`, NAT egress through compute's `bond0`

## Architecture

- **Self-contained secrets via sops-nix**. The VM decrypts `dotfiles-private/hosts/personal-agent/secrets.yaml` using its ed25519 SSH host key as the age identity (sops-nix converts ssh-ed25519 → age automatically). One key handles both SSH login and sops decryption; the host key persists on `/var/lib/hermes` across rebuilds.
- **Late activation in stage 2**. The volume isn't mounted in initrd, so sops can't find the SSH host key until userspace. `sops-late-activate.service` re-runs the (idempotent) activation script after `local-fs.target`, gated by the host key's presence.
- **`config.yaml` is non-secret**, rendered inside the VM from its own evaluated settings and installed via `ExecStartPre` post-mount (the upstream module's activation-time write gets shadowed by the volume mount).
- **Human admin only**. `bphenriques` is the sole sudoer; the `hermes` service user has no shell access, no wheel, no SSH keys. SSH from laptop via ProxyJump compute: `ssh personal-agent`.
- **Vault clone, not share**. `vault-sync.service` clones `bphenriques/notes` from gitea over SSH using the VM's host key as the authentication identity (same key as sops). Depth-1 clone on boot, refreshed every 5 min by `vault-sync.timer`. Clone is owned `root:hermes` mode 0750 — the agent reads but can't mutate. Any local change gets blown away on the next sync.

For the broader design context (microvm isolation rationale), see [`bphenriques-tools/microvm.md`](../../bphenriques-tools/microvm.md).

## Setup

This VM bootstraps off its own SSH host key. The key is generated on first boot and lives on the persistent volume; capturing it once and adding it as a sops recipient is the whole "key management."

### 1. Initial deploy (no secrets yet)

In `dotfiles-private`, expose an **empty** secrets file:

```nix
# dotfiles-private/flake.nix output
hosts.personal-agent.sopsSecretsFile = ./hosts/personal-agent/secrets.yaml;
```

Comment out anything in `hosts/personal-agent/default.nix` that reads from sops (or just `services.hermes-agent.enable = false;`) so the first boot can complete without decryption. Deploy:

```bash
nixos-rebuild switch --flake .#compute --target-host compute --use-remote-sudo
```

This brings up the VM, sshd generates its ed25519 host key at `/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key`.

### 2. Capture the host key as an age recipient

From the **laptop** (the alias is set up there with ProxyJump compute):

```bash
ssh personal-agent 'sudo cat /var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key.pub' | nix run nixpkgs#ssh-to-age
```

Or, if already SSH'd into the VM, drop the outer `ssh personal-agent`:

```bash
sudo cat /var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key.pub | nix run nixpkgs#ssh-to-age
```

Prints `age1...`. Add it to `dotfiles-private/.sops.yaml` as a recipient for `hosts/personal-agent/secrets.yaml`.

### 3. Create the encrypted secrets

```bash
sops edit dotfiles-private/hosts/personal-agent/secrets.yaml
```

```yaml
hermes-agent:
  api-server-key: <random 32 hex chars>
```

The VM's host key (already captured in step 2) doubles as the gitea auth identity, so there's no PAT or password to store.

### 4. Re-deploy with secrets enabled

Re-enable whatever you disabled in step 1, then:

```bash
nixos-rebuild switch --flake .#compute --target-host compute --use-remote-sudo
ssh compute 'sudo systemctl restart microvm@personal-agent'
```

On boot: VM mounts `/var/lib/hermes`, `sops-late-activate.service` fires once the host key path exists and runs the activation script, which decrypts secrets into `/run/secrets/`. `hermes-agent` starts and listens on `0.0.0.0:8642`. Traefik on compute proxies `hermes-api.{domain}` → VM.

`vault-sync.service` will fail with `Permission denied (publickey)` against gitea until step 5 is done — expected, since gitea doesn't yet know the VM's host key. `hermes-agent` waits on vault-sync (it requires the vault on disk), so it won't reach its socket until step 5 lands and the next timer fires (or you `systemctl restart vault-sync hermes-agent` after step 5).

### 5. Register the VM's host key with hermes-agent in gitea

vault-sync authenticates against gitea using the VM's ed25519 host key over SSH (port 2222). Gitea needs the public side registered to the `hermes-agent` user. Gitea's Site Admin UI doesn't let an admin add SSH keys to other users — keys are managed from each user's own Settings — so the one-time bootstrap is "set a temp password for hermes-agent, log in as them, add the key, optionally reset":

On **compute**:

```bash
# 1. Set a temp password (overrides the throw-away random from gitea-configure).
gitea-admin admin user change-password --username hermes-agent --password '<temp-pw>'
```

In the **gitea web UI** (basic-auth-disabled only blocks the *API* — the web login form still works):

```
2. Log in as hermes-agent with <temp-pw>.
3. Settings → SSH/GPG Keys → Add Key.
4. Paste the VM's host pubkey (the `ssh-ed25519 AAAA…` line from step 2 — the raw line,
   not the age form). Title: anything memorable, e.g. `personal-agent-host-key`.
5. Log out.
```

Optionally, on **compute**, reset hermes-agent's password back to throw-away:

```bash
gitea-admin admin user change-password --username hermes-agent --password "$(openssl rand -hex 32)"
```

Same key, three jobs: SSH-into-VM, sops-decrypt, gitea-clone. One trust anchor.

**Rotate:** regenerate the VM host key (rare; only on compromise) → re-run step 2 → re-run this step. The old key entry in gitea is left as-is or deleted from hermes-agent's Settings → SSH Keys.

**Revoke:** Settings → SSH/GPG Keys (logged in as hermes-agent, or via admin's API PAT) → Delete.

### 6. Smoke test

```bash
# SSH into the VM (alias defined in home-manager):
ssh personal-agent

# From inside, smoke trio:
hermes chat -q "What time is it in Lisbon?"
hermes chat -q "List the top-level folders in my notes vault."
hermes chat -q "Search my vault for car expenses."

# From the laptop, hit the API via Traefik:
curl -fsS -H "Authorization: Bearer <api-server-key>" https://hermes-api.<domain>/v1/models
```

The API token is the `hermes-agent/api-server-key` value from `dotfiles-private/hosts/personal-agent/secrets.yaml`.

## Operational notes

- **VM stuck initialising**: check `journalctl -u hermes-agent -n 50 --no-pager` (`bphenriques` is in `systemd-journal`). Common causes: Ollama unreachable from VM, pip lazy-installs on first chat (~1 min), or the `2 GiB qemu hang` if `mem` was set to exactly 2048.
- **GPU offload regressed**: check `nvidia-persistenced` on the laptop. CUDA-detection can fail across suspends; restart `ollama.service` to re-probe.
- **Vault stale / empty**: `systemctl status vault-sync.service` and `journalctl -u vault-sync`. Auth failures show as `git: fatal: Authentication failed` — verify `hermes-agent` still has read access to `bphenriques/notes` in gitea.
- **SSH host key changed after VM rebuild**: shouldn't happen — host keys persist on the volume — but if it does, `ssh-keygen -R 10.20.1.10` and re-accept once.
