# Agent VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) running hermes-agent as a personal assistant reachable via [NextChat](../../compute/selfhost/services/nextchat.nix) on compute.

Security concerns:

- No inference here; the model runs on the laptop, tools and memory stay local
- Egress is internet-only plus a single hole to `laptop:11434` (Ollama), never the rest of the LAN
- The vault is read-only, shared in over virtiofs from compute's gitea clone; the VM never reaches gitea
- The API (`:8642`) is bridge-only, gated by a key compute generates and shares in read-only over virtiofs; the VM holds no secrets of its own

## Ops

`ssh -J compute bphenriques@agent-vm`, then:

```bash
systemctl status hermes-agent      # the assistant runtime
journalctl -u hermes-agent -f      # tool calls and model errors
sudo -u hermes hermes chat         # CLI against the same state
```

The API key needs no sops: compute generates it (`selfhost.runtimeSecrets`), feeds NextChat locally, and shares the rendered `API_SERVER_KEY` env into the VM read-only. To rotate, delete `/var/lib/homelab-secrets/hermes-api-server-key` on compute and redeploy (the VM restarts to pick it up).

## Setup (one-time)

1. **gitea**: your Obsidian vault must live as `bphenriques/notes`, else `agent-vm-vault-sync` on compute no-ops.
2. **Deploy compute.** It builds and runs the guest, generates the API key, and shares it in; hermes reads it on start-up. The VM holds no secrets, so there is no `dotfiles-private` entry and no re-key dance.
