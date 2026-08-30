# Storage Host

NixOS NAS serving the household SMB shares from an encrypted ZFS mirror. Took over from a Synology
DS923+ on 2026-08-28.

## Hardware

- **Model**: Beelink ME Pro
- **CPU**: Intel N150 (4 E-cores)
- **RAM**: 16GB (ARC capped at 10GiB)
- **Pool**: 2 x 12TB Toshiba HDWG21C mirror (`tank`), native ZFS encryption, `recordsize=1M`
- **Root**: separate ext4 on NVMe, holds the pool key so `tank` unlocks unattended
- **Spare**: 2 x WD Red SN700 500GB, currently unused
- **UPS**: EATON Ellipse ECO 650 over USB (`usbhid-ups`)

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

## Operations

After a deploy or reboot:

```bash
systemctl --failed                                   # expect nothing
zpool status -P tank                                 # ONLINE, both members, zero errors
zfs get -H -o value keystatus tank                   # available, without being prompted
findmnt -t zfs -n -o TARGET                          # every declared mountpoint
ss -lnt                                              # 22, 445, 3493, 9100, 9134, 9633
```

`keystatus available` after a cold boot is the one that matters: it proves the pool key was read from
the unencrypted root without intervention. Storage mounts use `nofail`, so a missing pool leaves the
host reachable for repair while `selfhost-smb-permissions` and Samba fail closed rather than writing to
empty directories on the root NVMe. Never fix that by creating plain directories or relaxing the
mount guards.

Operator view:

```bash
sudo zpool status -x
sudo zfs list -r -o name,used,available,refer,mountpoint tank
sudo zfs get -r encryption,keystatus,compression,recordsize,snapdir tank
systemctl list-timers 'zfs-*' 'sanoid*' 'smartd*'
```

| Cadence   | Checks                                                                                                                                                      |
| --------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Monthly   | Scrub clean; SMART attributes and temperatures; B2 backup + verify on both hosts; pool capacity and snapshot growth                                         |
| Quarterly | Restore representative files from B2 and compare checksums; test Previous Versions from a client                                                            |
| Annually  | Kernel/ZFS update with console or JetKVM access (`nixos-rebuild test` first); test both ZFS key recovery paths; review HDD age and replacement availability |

Standing constraints:

- Keep at least 10% of `tank` free. Warn at 85%, treat 90% as urgent.
- No routine HDD spindown: stable temperatures and fewer power cycles suit an always-on host.
- No deduplication, L2ARC, SLOG or special vdev without measurements and an explicit failure model.
- Do not run `zpool upgrade` automatically; review the compatibility profile first.
- Native encryption protects removed drives, RMA and disposal, **not** a stolen complete box. A
  removable key can be adopted later with `zfs change-key`, at the cost of unattended recovery.
- **SMB signing and encryption stay unenforced, deliberately.** Clients negotiate signing on their own
  (compute's session runs `SMB3_11` with `AES-128-CMAC`), the LAN is trusted, and remote access arrives
  over WireGuard, which terminates on compute and reaches this host over that same trusted LAN. This is
  the posture the Synology shipped: server signing "Client defined", transfer encryption opt-in. Forcing
  either only pays off against an active on-LAN attacker, and `mandatory` risks locking out a client
  that does not sign.
- **The B2 application keys carry bucket-admin, accepted.** They are per-host already, but the console
  only offers presets and gives no way to narrow them to `listBuckets`, `listFiles`, `readFiles`,
  `writeFiles`, `deleteFiles`. Anything that reads a host's secrets can therefore delete that host's
  backups, which is the argument for keeping the sops key tight rather than for more B2 work.
- **The NVMe is the sensitive disk, not the HDDs.** It is unencrypted and holds the pool key, the sops
  age key, Samba's `passdb.tdb` and the journal. Disposal or RMA of the *root* NVMe needs
  `nvme format -s` or physical destruction; pulling the HDDs and wiping them is not enough. Dataset and
  snapshot names are also cleartext on the HDDs, since ZFS encrypts file contents and names but not
  pool metadata.

### Adding or removing a dataset

`nixos-rebuild` never creates or destroys datasets; it only emits the mount. Do the ZFS half by hand,
and mind the order.

**Adding, dataset first.** A share whose mount is missing fails `selfhost-smb-permissions`, which Samba
requires, so it takes down every share rather than just the new one.

```bash
sudo zfs create -o mountpoint=/srv/storage/archive tank/archive
```

Then declare it in `disko/pool.nix`, add a share in `shares/default.nix` if it should be exported, and
rebuild. `dataset` and `childDatasets` derive themselves, and a dataset nested under an existing share
is adopted by it without a share of its own.

**Removing, config first.** Drop the share, then the dataset from `disko/pool.nix`, rebuild, and only
then:

```bash
sudo zfs destroy -r tank/archive
```

Nothing prunes what you leave behind: sanoid's `autoprune` only manages datasets still in its config, so
orphaned snapshots persist, and B2 keeps the last copy until retention expires.

Half-finished in either direction is caught at eval: a share with no dataset is an assertion, a mounted
dataset reached by no share is a warning naming it.

### Degraded pool

Preserve evidence before acting:

```bash
sudo zpool status -P tank
sudo smartctl -x /dev/disk/by-id/<failed-or-suspect-disk>
journalctl -k -b | grep -Ei 'zfs|ata|error|fail|reset'
```

The bays are not labelled, so `zpool status -P` gives the failed drive's serial but not where it sits.
Read the serial from the by-id path (they are declared in `disko/pool.nix`), power down, open
both bays and match it against the stickers.

Do not use `zpool add` when replacing a mirror member. Update its by-id entry, inspect a pool-only
Disko format dry run, partition the replacement, then `zpool replace` and monitor the resilver. Do not
detach the surviving member. A mirror resilvers from the survivor, so one failure means one
replacement; the shared age and batch is a reason to watch the survivor closely, not to swap both.

## Deferred work

- Keep one SN700 as a cold spare and give the other a `fast` pool for ephemeral scratch. `zpool trim`
  becomes meaningful there, where today it is inert against the HDD mirror.
- Decide whether compute gains a second backup target on `fast` once it exists, a local copy alongside
  B2, or whether off-site alone stays sufficient.
- Encrypt the root NVMe rather than moving the ZFS key to removable USB. A USB key left plugged in has
  the same threat model as today, and unplugged it costs unattended recovery. The box has TPM2 and
  Secure Boot in setup mode, so LUKS with `systemd-cryptenroll --tpm2-device=auto` keeps unattended
  boot. Without measured boot, PCR7 is forgeable, so this protects a pulled disk and not a stolen box,
  which is exactly the RMA and disposal case above.
- Consider NFS for selected machine consumers now that SMB is stable.
- Revisit per-user `refquota` only under real capacity pressure.
