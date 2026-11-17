# Agent VM

A **sealed** cloud-hypervisor microVM on [`ai`](../../..) running `hermes-agent` as a personal assistant reachable via [the chat UI](../../../../compute/selfhost/services/open-webui) on compute:
- Inference stays on the host, reached through the bridge gateway; tools and memory stay in the guest
- Egress is internet-only plus five holes: the gateway on `11434` and `8090` (inference, whisper), `storage:445` (its NAS doors), `compute:2222` (gitea's SSH) and `compute:443` (Radicale's `dav.`), never the rest of the LAN
- The vault and recipes come from the NAS as its own SMB principal, `machine-agent-alfredo-vm`, through doors that expose only those folders ([storage](../../../../storage/README.md))
- The API (`:8642`) admits only compute (`ingressFrom`), which routes to ai's bridge and fronts it with Traefik. The key is generated on compute and copied into this guest's sops, like the SMB password; the one credential of its own is the Cookidoo token (see below)

## What the agent may do to the vault

Full read and write, including delete and rename. Two layers grant it, and each fails differently,
so check them in this order:

| layer                              | what it grants                                                                                                                          | how a failure reads            |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------ |
| SMB door on storage                | `machine-agent-alfredo-vm` is granted `rw` on `notes` only; the mount forces `gid=5000` with `0660`, so hermes's `vault` group is the write permission | mount fails, or `EACCES`       |
| `ReadWritePaths` on `hermes-agent` | `ProtectSystem = "strict"` makes everything read-only, so the vault path is listed explicitly                                           | `EROFS`, read-only file system |

Nothing withholds deletion. A client-side `tools.exclude` used to, but mcpvault offers its tools
regardless, so it only ever hid them from the model. Recovery is the real backstop: the B2 backup of
the share, plus CouchDB's retained revisions.

Filing conventions live in the vault's own root `AGENTS.md`, so they are edited in Obsidian rather
than redeployed.

The share is also mounted `noexec`, and the sync daemon ignores every dotted path, so `.obsidian/`
plugin code never enters the vault sync and is not reachable this way.

## Calendars

`calendars-sync` keeps the calendars the agent reads and writes as a vdir in `/var/lib/hermes/calendars`, 2 minutes
after boot and every 30 minutes, as `hermes`. A failed sync shows as a failed unit.

- **Radicale**: every collection of the owner named in dotfiles-private, as compute's `personal-agent`. The owner's
  grant lets it change items and add collections, never delete, replace, rename or share one, and every change is a
  commit on compute ([selfhost-nix](https://github.com/bphenriques/selfhost-nix/blob/main/docs/src/apps.md#radicale)).
  Compute generates its password, copied into this guest's secrets.
- **Google**: the shared calendar, through an OAuth client of the account.

## The agent

What runs here is [Alfredo](https://github.com/bphenriques/agents-alfredo): its persona, skills and tool policy live there. This
folder deploys it: the model and endpoints, the shares, the state volume and the seal. The private `household.md` is
appended to its `SOUL.md`, and the Cookidoo collection comes from dotfiles-private.

To iterate on it, deploy with `--override-input agents-alfredo ~/workdir/agents-alfredo`.

## Forge access

Git over SSH to gitea on compute, as the `personal-agent` service account declared in
[compute's selfhost config](../../../../compute/selfhost/default.nix).

- The keypair is generated here, as `hermes` under `/var/lib/hermes/.ssh`, so it sits on the state volume
  and survives reboots; root's home does not. Only the public half is declared on compute.
- Remotes use `ssh://gitea@<compute-ip>:2222/<owner>/<repo>.git`. The login is gitea's `RUN_USER`.
- The hole is `egress.allowLan` in ai's [microvm/host.nix](../../host.nix), for that one port.

## Accepted trade-offs

- **The agent can be steered by what it reads.** Web pages, captions and notes reach a model with a
  terminal, write and delete on the vault and recipes, push to its repo and open internet egress, so a
  prompt injection can alter notes or send them out. The VM, the folder-scoped NAS principal and the B2
  backup bound the damage; nothing prevents it.
- **The agent can read the Google token.** The sync runs as `hermes`, and a sandbox would not hold: hermes's user
  manager starts processes outside it. The token reaches every calendar of the account, sharing included; only the
  sync's `collections` limits it to the shared one. Deleted events stay in Google's bin for 30 days; edits have no history.
- **No backup of `/var/lib/hermes`.** It holds the agent's memory, sessions and tokens (~50M), all
  rebuildable: what matters lives on the NAS, which is backed up. Losing it means a new SSH host key, so
  re-key the guest's sops file from the laptop (`base-microvm`), sign in to Cookidoo and redo the forge key.
- **The API key crosses the LAN in plain HTTP** on the compute to ai hop; clients reach Traefik over TLS.
  Reading it takes an on-path attacker on the LAN, and the guest admits the API only from compute.
- **ai's guest subnet is routable.** The guest's firewall is the single gate: a LAN host routed through
  ai reaches none of its ports (tested from storage), compute reaches only `9100` and the API.

## Ops

```bash
ssh -J ai bphenriques@agent-alfredo-vm

systemctl status hermes-agent      # the assistant runtime
journalctl -u hermes-agent -f      # tool calls and model errors
sudo -u hermes hermes chat         # CLI against the same state
sudo systemctl start cookidoo-sync # sync the Cookidoo collection now
sudo systemctl start calendars-sync # sync the calendars now
```

To rotate the API key, delete `/var/lib/homelab-secrets/hermes-api-server-key` on compute, deploy it, and copy the new value into this guest's `secrets.yaml` as `hermes.api-key`.

To rotate the Google client secret, add a secret to the client (Google Auth Platform, Clients), put it in `secrets.yaml` as `calendar.google.clientSecret`, deploy, check a sync, then disable the old one. To replace the token, remove Alfredo at myaccount.google.com/connections, delete `/var/lib/hermes/vdirsyncer/google-token.json` and sign in again.

## Setup (one-time)

1. **Storage** generates the SMB password (`/var/lib/homelab-secrets/smb-password-machine-agent-alfredo-vm`). Put it in `dotfiles-private/hosts/agent-alfredo-vm/secrets.yaml` as `smb.password` and `sops encrypt -i` it; the guest decrypts with its SSH host key.
2. **API key**: copy `/var/lib/homelab-secrets/hermes-api-server-key` from compute into the same file as `hermes.api-key`, then deploy ai.
3. **Forge**: generate the keypair here (`sudo -u hermes ssh-keygen -t ed25519 -N "" -C personal-agent@agent-alfredo-vm -f /var/lib/hermes/.ssh/id_ed25519`), declare the public half on compute, pin the host key (`sudo -u hermes sh -c 'ssh-keyscan -p 2222 <compute-ip> >> /var/lib/hermes/.ssh/known_hosts'`), then add `personal-agent` to the repository with write access in gitea, which the framework does not provision. Verify with `sudo -u hermes ssh -p 2222 -T gitea@<compute-ip>`.
4. **Cookidoo**: sign in once with `sudo -u hermes cookidoo login`.
5. **Radicale**: deploy compute, which generates the password of its `personal-agent` account. Put the output of `sudo homelab-secrets cat radicale-password-personal-agent` there in this guest's `secrets.yaml` as `radicale.password`, then deploy ai.
6. **Google OAuth client**: in a Google Cloud project of the account, enable the CalDAV API. In Google Auth Platform, register an External app whose home page and privacy policy are Alfredo's README, publish it (in Testing the token expires every 7 days), and create a Desktop client. Put its id and secret in `secrets.yaml` as `calendar.google.clientId` and `calendar.google.clientSecret`.
7. **Google sign-in**, again only if Google revokes the token (removed from the account, or 6 months unused):
   1. `sudo -u hermes vdirsyncer-alfredo discover` prints a Google URL and waits.
   2. Open it on the laptop and approve: at "Google hasn't verified this app", Advanced, then Go to Alfredo. The browser then fails to load `http://127.0.0.1:<port>/?code=…`; copy that address.
   3. In a second SSH session here, `curl '<address>'`, quoted: unquoted, the shell cuts it at the first `&` and `discover` fails with `missing_code`.
   4. `sudo systemctl start calendars-sync`, then check: `sudo -u hermes sh -c 'for d in /var/lib/hermes/calendars/*/; do echo "$(cat "$d/displayname") ($(ls "$d" | grep -c "\.ics$") events)"; done'`.
