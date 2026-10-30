# Agent VM

A **sealed** cloud-hypervisor microVM on [`compute`](../../compute) running `hermes-agent` as a personal assistant reachable via [the chat UI](../../compute/selfhost/services/open-webui) on compute:
- No inference here; the model runs on the [`ai`](../../ai) host, tools and memory stay local
- Egress is internet-only plus two holes, `ai:11434` and `ai:8090` (Ollama, whisper) and the host's bridge address on `2222` (gitea's SSH), never the rest of the LAN
- Curated set of RW directories from the live NAS 
- The API (`:8642`) and the admin console (`:8787`) are bridge-only, both fronted by compute's Traefik: the API gated by a key compute generates, the console by Pocket ID. Credentials arrive read-only over virtiofs; the one credential of its own is the Cookidoo token (see below)

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

## The agent

What runs here is [Alfred](../../../agents/alfred): its persona, skills and tool policy live there. This
folder deploys it: the model and endpoints, the shares, the state volume and the seal.

## Forge access

Git over SSH to gitea on compute, as the `personal-agent` service account declared in
[compute's selfhost config](../../compute/selfhost/default.nix).

- The keypair is generated here, as `hermes` under `/var/lib/hermes/.ssh`, so it sits on the state volume
  and survives reboots; root's home does not. Only the public half is declared on compute.
- Remotes use `ssh://gitea@10.20.1.1:2222/<owner>/<repo>.git`: the bridge address rather than the public
  name, which resolves to compute's LAN address and stays dropped. The login is gitea's `RUN_USER`.
- The hole is `egress.allowHostPorts` (gitea's SSH port) in compute's [microvm/host.nix](../../compute/microvm/host.nix), pinned
  to the bridge address so the same port on compute's LAN address remains unreachable.

## Admin console

[hermes-webui](https://github.com/nesquena/hermes-webui) at `https://hermes-webui.<domain>`, which is the
only surface onto the agent's memory, skills, cron jobs and sessions: the OpenAI-compatible API the chat UI
speaks has no concept of them. It is a front end over the same `HERMES_HOME`, not a second agent, so it runs
as `hermes` and its own state sits in `webui/` inside that home.

Its password auth is unused: Pocket ID is the gate, and the stored `password_hash` in
`$HERMES_HOME/webui/settings.json` is what to clear if it ever comes back (the env var outranks it, the file
is the fallback).

## Ops

```bash
ssh -J compute bphenriques@agent-alfred-vm

systemctl status hermes-agent      # the assistant runtime
journalctl -u hermes-agent -f      # tool calls and model errors
sudo -u hermes hermes chat         # CLI against the same state
```

The API key needs no sops: compute generates it (`selfhost.runtimeSecrets`), feeds the chat UI locally, and shares the rendered `API_SERVER_KEY` env into the VM read-only. To rotate, delete `/var/lib/homelab-secrets/hermes-api-server-key` on compute and redeploy (the VM restarts to pick it up).

## Setup (one-time)

1. **Shares**: compute must have the `bphenriques` and `media` SMB shares mounted; virtiofsd will not start without them, so the guest will not boot while the NAS is down.
2. **Deploy compute.** It builds and runs the guest, generates the API key, and shares it in; hermes reads it on start-up. There is no sops file and no re-key dance; `dotfiles-private` holds only non-secret household facts.
3. **Forge**: generate the keypair here (`sudo -u hermes ssh-keygen -t ed25519 -N "" -C personal-agent@agent-alfred-vm -f /var/lib/hermes/.ssh/id_ed25519`), declare the public half on compute, pin the host key (`sudo -u hermes sh -c 'ssh-keyscan -p 2222 10.20.1.1 >> /var/lib/hermes/.ssh/known_hosts'`), then add `personal-agent` to the repository with write access in gitea, which the framework does not provision. Verify with `sudo -u hermes ssh -p 2222 -T gitea@10.20.1.1`.
4. **Skills**: after the first deploy with the allowlist, drop the previously seeded catalog with `sudo -u hermes hermes skills opt-out --remove` (it deletes only unmodified bundled skills), then remove whatever `sudo -u hermes hermes skills list` still shows outside the allowlist.
5. **Cookidoo**: sign in once, see [Alfred](../../../agents/alfred/README.md#meals-cozinha).
