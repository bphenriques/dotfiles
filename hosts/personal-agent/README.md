# personal-agent

Microvm hosting the homelab's personal assistant (currently [hermes-agent](https://hermes-agent.nousresearch.com/)). Isolated from compute via its own kernel (qemu); reads the Obsidian vault by cloning over SSH from gitea; separate sops trust domain.

> **Status (2026-06):** Experimental, paused. Compute-tier inference on the N150 is too slow for interactive use; the work is paused pending hardware upgrade or shift to on-device models. The microvm + ntfy + Traefik infrastructure remains as a foundation if/when the workload returns.

For the routing model, hardware constraints, and the voice-memo pipeline design that this VM was built around, see [`voice-memo-pipeline.md`](../../voice-memo-pipeline.md) at the repo root.

## Hardware

- **Hypervisor**: qemu, 2 vCPU, 4 GB RAM (`2048` triggers a known qemu hang — see microvm.nix issue #171).
- **Persistent volume**: 4 GB at `/var/lib/hermes` — agent state, vault clone, SSH host keys.
- **Network**: bridge `compute-microvm` on compute; VM at `10.20.1.10/24`; NAT egress through compute's `bond0`.

## Architecture

- **One key for everything**: the VM's ed25519 SSH host key is the SSH identity (`bphenriques` login), the sops age identity (sops-nix converts ssh-ed25519 → age automatically), *and* the gitea auth key for the vault clone. Persisted on the volume; survives rebuilds.
- **Late sops activation**: `/var/lib/hermes` isn't mounted in initrd, so the activation script can't reach the host key there. `sops-late-activate.service` re-runs activation in stage 2 once the path exists.
- **Read-only vault**: `vault-sync.service` clones `bphenriques/notes` from gitea over SSH (port 2222), refreshes every 5 minutes. Owned `root:hermes`, mode 0750 — agent reads, can't mutate. Writes go through the PR flow in `voice-memo-pipeline.md`.
- **Admin / agent separation**: `bphenriques` is the sole sudoer; the `hermes` service user has no shell, sudo, or SSH.

## Setup

This VM bootstraps off its own SSH host key. The key is generated on first boot; capturing it once and adding it as a sops recipient is the whole "key management."

### 1. Initial deploy (no secrets)

In `dotfiles-private` expose an empty secrets file:
```nix
hosts.personal-agent.sopsSecretsFile = ./hosts/personal-agent/secrets.yaml;
```
Comment out `services.hermes-agent.enable = false;` (or anything reading from sops) in `default.nix` so the first boot completes. Deploy compute. sshd generates the ed25519 host key at `/var/lib/hermes/.ssh-host-keys/`.

### 2. Capture the host key

From the laptop (ProxyJump alias resolves) or from inside the VM:
```bash
ssh personal-agent 'sudo cat /var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key.pub' | nix run nixpkgs#ssh-to-age
```
Add the printed `age1…` to `dotfiles-private/.sops.yaml` as a recipient for `hosts/personal-agent/secrets.yaml`.

### 3. Create the encrypted secrets

```bash
sops edit dotfiles-private/hosts/personal-agent/secrets.yaml
```
```yaml
hermes-agent:
  api-server-key: <random 32 hex chars>
```

### 4. Re-deploy with secrets enabled, then register the host key with gitea

Re-enable hermes-agent in `default.nix`, deploy compute. `vault-sync` will fail with `Permission denied (publickey)` until step 5 — expected.

Register the VM's host pubkey (raw `ssh-ed25519 AAAA…` line, not the age form) against the `hermes-agent` gitea user. Gitea's Site Admin UI doesn't expose per-user SSH-key management, so:
```bash
sudo gitea-admin admin user change-password --username hermes-agent --password '<temp>'
# Log in to gitea web UI as hermes-agent → Settings → SSH/GPG Keys → Add Key
sudo gitea-admin admin user change-password --username hermes-agent --password "$(openssl rand -hex 32)"
```

After the key lands: `ssh personal-agent 'sudo systemctl restart vault-sync hermes-agent'`.

### 5. Smoke test

```bash
ssh personal-agent
hermes chat -q "What time is it in Lisbon?"
hermes chat -q "List the top-level folders in my notes vault."

# From laptop, hit the API via Traefik:
curl -fsS -H "Authorization: Bearer <api-server-key>" https://hermes-api.<domain>/v1/models
```

## Operational notes

- **VM stuck initialising**: `journalctl -u hermes-agent` (`bphenriques` is in `systemd-journal`). Common causes: Ollama unreachable from VM, first-run pip lazy-install (~1 min).
- **Vault stale / empty**: `systemctl status vault-sync` and its journal. Auth failures (`Permission denied (publickey)`) mean the gitea-side key registration drifted — re-do step 4.
- **Host key rotation**: regenerate on the VM, re-do steps 2 (sops recipient) and 4 (gitea key). The old key entries can be revoked at leisure.
