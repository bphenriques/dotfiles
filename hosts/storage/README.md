# Storage Server

NixOS based NAS hosting the data supporting my homelab.

## Hardware

- **Model**: Beelink ME Pro
- **CPU**: Intel N150 (4 E-cores, shared CPU/iGPU die)
- **RAM**: 16GB
- **Storage**: 2 x 12TB Toshiba HDWG21C mirror (`tank`), native ZFS encryption, `recordsize=1M`
- **UPS**: EATON Ellipse ECO 650 over USB

## Architecture

```
  LAN clients ──SMB(445)──▶ Samba ──▶ /srv/storage/<share>
  (+ Previous Versions)                    │
                                           ▼
                          tank  ─ 2 x 12TB mirror, encrypted
                            ├── users/<person>     one dataset each
                            ├── media ── books, gaming, music
                            │     movies/tv/downloads share the parent
                            │     dataset so hardlinks work
                            └── shared
                                           │
  sanoid ──────────────────────────────────┤ snapshots → .zfs → Previous Versions
                                           │
  rustic (03:00) ──────────────────────────┴───▶ Backblaze B2
                                                 (repo shared with compute)

  node + smartctl + zfs exporters ────────────▶ compute Prometheus
  NUT upsd (3493) ────────────────────────────▶ compute upsmon
  UPS ──USB──▶ storage
```

Alerting lives on compute by decision: a NAS should not be the thing that notices it is down.

## Shares

- **Household**: `media` and `shared`. **Personal**: one dataset each under `tank/users`.
- **Private settings** own personal share names, owners, Samba access and the 5GbE MAC. Datasets and
  their children are read back from `disko/pool.nix`, so the pool layout is declared once. The share
  inventory in `shares/` is transport-neutral; the SMB view over it is `services/samba.nix`.
- **Principals** are `selfhost.users` and `selfhost.serviceAccounts` entries with `storage.smb.enable`.
  Holding an account is separate from being let into a share: grants live on the share, and a share can
  be owned by a principal holding none. Clients must speak SMB 3.1.1.
- **Permissions**: shares are `2770` setgid owned by `storage-<name>`. Samba drops to the connecting
  user, so `/srv/storage` itself must stay `0755` root-owned; `selfhost-smb-permissions` asserts that on
  every boot.
- **Previous Versions** reaches back about five days. `shadow:format` must match a whole snapshot
  name, so only sanoid's `_hourly` snapshots surface; dropping the suffix to catch `_daily` matches
  nothing.

## Setup

| Dependency   | What                                                   | Reference                                                  |
| ------------ | ------------------------------------------------------ | ---------------------------------------------------------- |
| Backblaze B2 | Bucket + application key with list/read/write/delete   |                                                            |
| ntfy         | Publisher token, provisioned on compute                | [compute](../compute/README.md)                            |
| Private      | Share names, owners, access, 5GbE MAC, `notify.url`    | `dotfiles-private/hosts/storage/settings.nix`              |
| Secrets      | Bootstrap via `dotfiles-secrets init-host` (Bitwarden) | [`apps/nixos-install`](../../apps/nixos-install/README.md) |

- Disko is the single declaration for pool datasets and runtime mounts.
- A normal `nixos-rebuild` does **not** format. `nixos-anywhere` runs the host's own disko script and
  formats every declared device, root and pool alike.
- Pool destruction, vdev changes and dataset deletion stay manual.

## Backups

- One B2 bucket and rustic repository **shared with compute**. Snapshots carry a `target:<name>` tag
  and each host's `forget` is filtered to its own host and tag, so neither ages out the other's.
- **Only storage prunes.** rustic takes no repository lock, and two concurrent prunes read packs the
  other is deleting. Compute still forgets; storage reclaims its space too, because prune reachability
  spans the whole repository. A third host joining must set `prune = false`.
- Reading datasets locally records real ZFS ownership. Compute's older CIFS-era snapshots recorded a
  fabricated `root:homelab-*` whose groups do not exist here, so those need `--no-ownership` on
  restore. Storage's own snapshots need nothing.
- Notifications publish to compute's ntfy, titled by host. Read the token out on compute once with
  `sudo cat /var/lib/homelab-secrets/notify-publishers/storage-backup`, then store it as
  `notify/backup-token` in this host's private secrets.

Restores use the same generated profile as the timer. Inspect first, restore each tree separately,
never with `--delete`:

```bash
sudo rustic -P backblaze snapshots
sudo rustic -P backblaze ls -l <snapshot-id>:/var/lib/homelab-backup/backblaze/src/nas/shared | head
sudo install -d -m 0700 /srv/restore-check
sudo rustic -P backblaze restore \
  <snapshot-id>:/var/lib/homelab-backup/backblaze/src/nas/shared \
  /srv/restore-check/shared
```

The destination directory is rustic's own container and takes no metadata from the snapshot, so it
lands `root:root`; everything inside carries the recorded owner, group and mode. Pin a known-good
snapshot with `sudo rustic -P backblaze tag --add keep-forever <snapshot-id>`.

