# Agent VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) running hermes-agent as a personal assistant reachable via [the chat UI](../../compute/selfhost/services/open-webui) on compute.

Security concerns:

- No inference here; the model runs on the [`ai`](../../ai) host, tools and memory stay local
- Egress is internet-only plus a single hole to `ai:11434` (Ollama), never the rest of the LAN
- The vault is the live NAS copy, shared in RW over virtiofs from compute's CIFS mount; write access is group gid 5000, nothing else
- The API (`:8642`) is bridge-only, gated by a key compute generates and shares in read-only over virtiofs; the VM holds no secrets of its own

## What the agent may do to the vault

Full read and write, including delete and rename. Two layers grant it, and each fails differently,
so check them in this order:

| layer                              | what it grants                                                                                                                          | how a failure reads            |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------ |
| CIFS mount on compute              | the share forces `gid=5000` with `0660`, so `users.groups.vault.gid = 5000` here with hermes in it is the whole of the write permission | `EACCES`, permission denied    |
| `ReadWritePaths` on `hermes-agent` | `ProtectSystem = "strict"` makes everything read-only, so the vault path is listed explicitly                                           | `EROFS`, read-only file system |

Nothing withholds deletion. A client-side `tools.exclude` used to, but mcpvault offers its tools
regardless, so it only ever hid them from the model. Recovery is the real backstop: the B2 backup of
the share, plus CouchDB's retained revisions.

Filing conventions live in the vault's own root `AGENTS.md`, so they are edited in Obsidian rather
than redeployed.

The share is also mounted `noexec`, and the sync daemon ignores every dotted path, so `.obsidian/`
plugin code never enters the vault sync and is not reachable this way.

## Ops

The fleet-wide SSH profile disables TCP forwarding, so `-J` cannot reach the bridge; relay through compute instead:

```bash
ssh -o ProxyCommand='ssh root@compute nc %h %p' bphenriques@agent-vm

systemctl status hermes-agent      # the assistant runtime
journalctl -u hermes-agent -f      # tool calls and model errors
sudo -u hermes hermes chat         # CLI against the same state
```

The API key needs no sops: compute generates it (`selfhost.runtimeSecrets`), feeds the chat UI locally, and shares the rendered `API_SERVER_KEY` env into the VM read-only. To rotate, delete `/var/lib/homelab-secrets/hermes-api-server-key` on compute and redeploy (the VM restarts to pick it up).

## Setup (one-time)

1. **Vault**: compute must have the `bphenriques` SMB share mounted; virtiofsd will not start without it, so the guest will not boot while the NAS is down.
2. **Deploy compute.** It builds and runs the guest, generates the API key, and shares it in; hermes reads it on start-up. The VM holds no secrets, so there is no `dotfiles-private` entry and no re-key dance.
