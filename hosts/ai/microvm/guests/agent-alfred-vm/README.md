# Agent VM

A **sealed** cloud-hypervisor microVM on [`ai`](../../..) running `hermes-agent` as a personal assistant reachable via [the chat UI](../../../../compute/selfhost/services/open-webui) on compute:
- Inference stays on the host, reached through the bridge gateway; tools and memory stay in the guest
- Egress is internet-only plus four holes: the gateway on `11434` and `8090` (Ollama, whisper), `storage:445` (its NAS doors) and `compute:2222` (gitea's SSH), never the rest of the LAN
- The vault and recipes come from the NAS as its own SMB principal, `machine-agent-alfred-vm`, through doors that expose only those folders ([storage](../../../../storage/README.md))
- The API (`:8642`) admits only compute (`ingressFrom`), which routes to ai's bridge and fronts it with Traefik. The key is generated on compute and copied into this guest's sops, like the SMB password; the one credential of its own is the Cookidoo token (see below)

## What the agent may do to the vault

Full read and write, including delete and rename. Two layers grant it, and each fails differently,
so check them in this order:

| layer                              | what it grants                                                                                                                          | how a failure reads            |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------ |
| SMB door on storage                | `machine-agent-alfred-vm` is granted `rw` on `notes` only; the mount forces `gid=5000` with `0660`, so hermes's `vault` group is the write permission | mount fails, or `EACCES`       |
| `ReadWritePaths` on `hermes-agent` | `ProtectSystem = "strict"` makes everything read-only, so the vault path is listed explicitly                                           | `EROFS`, read-only file system |

Nothing withholds deletion. A client-side `tools.exclude` used to, but mcpvault offers its tools
regardless, so it only ever hid them from the model. Recovery is the real backstop: the B2 backup of
the share, plus CouchDB's retained revisions.

Filing conventions live in the vault's own root `AGENTS.md`, so they are edited in Obsidian rather
than redeployed.

The share is also mounted `noexec`, and the sync daemon ignores every dotted path, so `.obsidian/`
plugin code never enters the vault sync and is not reachable this way.

## The agent

What runs here is [Alfred](../../../../../agents/alfred): its persona, skills and tool policy live there. This
folder deploys it: the model and endpoints, the shares, the state volume and the seal.

## Forge access

Git over SSH to gitea on compute, as the `personal-agent` service account declared in
[compute's selfhost config](../../../../compute/selfhost/default.nix).

- The keypair is generated here, as `hermes` under `/var/lib/hermes/.ssh`, so it sits on the state volume
  and survives reboots; root's home does not. Only the public half is declared on compute.
- Remotes use `ssh://gitea@<compute-ip>:2222/<owner>/<repo>.git`. The login is gitea's `RUN_USER`.
- The hole is `egress.allowLan` in ai's [microvm/host.nix](../../host.nix), for that one port.

## Accepted trade-offs

- **No backup of `/var/lib/hermes`.** It holds the agent's memory, sessions and tokens (~50M), all
  rebuildable: what matters lives on the NAS, which is backed up. Losing it means a new SSH host key, so
  re-key the guest's sops file from the laptop (`base-microvm`), sign in to Cookidoo and redo the forge key.
- **The API key crosses the LAN in plain HTTP** on the compute to ai hop; clients reach Traefik over TLS.
  Reading it takes an on-path attacker on the LAN, and the guest admits the API only from compute.
- **ai's guest subnet is routable.** The guest's firewall is the single gate: a LAN host routed through
  ai reaches none of its ports (tested from storage), compute reaches only `9100` and the API.

## Ops

```bash
ssh -J ai bphenriques@agent-alfred-vm

systemctl status hermes-agent      # the assistant runtime
journalctl -u hermes-agent -f      # tool calls and model errors
sudo -u hermes hermes chat         # CLI against the same state
```

To rotate the API key, delete `/var/lib/homelab-secrets/hermes-api-server-key` on compute, deploy it, and copy the new value into this guest's `secrets.yaml` as `hermes.api-key`.

## Setup (one-time)

1. **Storage** generates the SMB password (`/var/lib/homelab-secrets/smb-password-machine-agent-alfred-vm`). Put it in `dotfiles-private/hosts/agent-alfred-vm/secrets.yaml` as `smb.password` and `sops encrypt -i` it; the guest decrypts with its SSH host key.
2. **API key**: copy `/var/lib/homelab-secrets/hermes-api-server-key` from compute into the same file as `hermes.api-key`, then deploy ai.
3. **Forge**: generate the keypair here (`sudo -u hermes ssh-keygen -t ed25519 -N "" -C personal-agent@agent-alfred-vm -f /var/lib/hermes/.ssh/id_ed25519`), declare the public half on compute, pin the host key (`sudo -u hermes sh -c 'ssh-keyscan -p 2222 <compute-ip> >> /var/lib/hermes/.ssh/known_hosts'`), then add `personal-agent` to the repository with write access in gitea, which the framework does not provision. Verify with `sudo -u hermes ssh -p 2222 -T gitea@<compute-ip>`.
4. **Skills**: after the first deploy with the allowlist, drop the previously seeded catalog with `sudo -u hermes hermes skills opt-out --remove` (it deletes only unmodified bundled skills), then remove whatever `sudo -u hermes hermes skills list` still shows outside the allowlist.
5. **Cookidoo**: sign in once, see [Alfred](../../../../../agents/alfred/README.md#meals-cozinha).
