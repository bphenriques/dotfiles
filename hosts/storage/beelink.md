# Beelink ME Pro (`storage`)

Design for the NixOS host replacing the Synology DS923+. Storage only, plus backup cron. No
selfhost-nix services: storage stays separate from compute by design.

Not yet built. Open items at the bottom. The framework changes this needs, and the `custom.paths`
refactor it enables, are in [`storage-model.md`](storage-model.md).

## Hardware

- Intel N150, 16GB RAM
- 2x internal HDD bays (2x 12TB), 3x NVMe slots
- 5GbE: Realtek **RTL8126A**; 2.5GbE: Intel **i226-V**

Both NICs are supported on the pinned 6.18 LTS, checked against the 6.18.40 source rather than a
changelog: `r8169_main.c` carries `RTL8126A` at PCI `0x8126` (`RTL_GIGA_MAC_VER_70`), and
`igc_hw.h` has `IGC_DEV_ID_I226_V = 0x125C`. **RTL8126A loads firmware** (`rtl_nic/rtl8126a-2.fw`,
`-3.fw`), so `hardware.enableRedistributableFirmware = true` is mandatory here.

A 2-disk mirror sustains ~150-260 MB/s: it feeds 2.5GbE and gets about a third of 5GbE. That is a
property of the disks, not the filesystem. The 5GbE port only pays off for NVMe-backed or cached
data.

12TB mirrored gives 12TB usable against ~2.4TB in use today (media 2.1T, bphenriques 284G), so
roughly 20% full at migration.

## Disks

| Device               | Role                  | Encryption |
| -------------------- | --------------------- | ---------- |
| NVMe1, 1TB bundled   | ESP 512M + ext4 root  | none       |
| NVMe2 + NVMe3, 512GB | zpool `fast` (mirror) | ZFS native |
| HDD1 + HDD2, 12TB    | zpool `tank` (mirror) | ZFS native |

All three slots used. 1TB is far more than the root needs, but it is the drive that comes with the
box and using it there leaves both 512GB devices free to mirror.

NVMe1 stays unencrypted so the box boots unattended after a power cut, and holds the sops age key
plus the ZFS keys for both pools. Same posture as compute, whose root is also plain. Threat model is
drives leaving the house (RMA, resale, disposal), which the HDDs satisfy.

Root is reproducible from the flake, so its lack of redundancy costs a reinstall at worst.

**`fast` being mirrored upgrades what it can hold.** The earlier single-device plan meant everything
on it had to be reproducible or already a copy elsewhere. Mirrored, `fast/backup` becomes a genuine
local backup target rather than a best-effort one.

The figures below are for the pair currently in the Synology, a RAID1 storage pool that held Docker
data and now has no volume. **If a different 512GB pair is used, re-read SMART on those instead.**
DSM reports one of these as degraded; `nvme smart-log` (2026-08-10) says otherwise for both:

|                    | nvme0n1 | nvme1n1 |
| ------------------ | ------- | ------- |
| `critical_warning` | 0       | 0       |
| `media_errors`     | 0       | 0       |
| `available_spare`  | 100%    | 100%    |
| `percentage_used`  | 19%     | 19%     |
| `power_on_hours`   | 19,313  | 18,912  |

They are WD Red SN700 500GB, rated **1000 TBW**. Host writes are ~158 TB each (308.77M data units at
512,000 bytes per the NVMe spec), so 15.8% of the rating against a reported 19% used; the gap is
write amplification, since the controller's internal writes consume endurance without appearing in
`data_units_written`. WAF around 1.2 is healthy.

The observed rate is ~196 GB/day, which is Synology storage-pool duty for Docker. As a `fast` pool
holding a syncthing database, a rustic cache and a backup target, writes drop by an order of
magnitude and endurance stops being a consideration.

**DSM's "degraded" is not a hardware fault**, most likely its compatibility database, since
third-party NVMe needs `Synology_HDD_db` to be accepted at all. Mirror them.

Their `data_units_written` differ by 1,610 out of 308 million, so they wore in lockstep and will
reach end of life together. That is the standing risk of any same-model, same-age mirror, and the
`SMARTHighWearLevel` alert already in the monitoring scope (`percentage_used > 80`) is what catches
it in time to replace one.

DSM's own shell cannot produce this: its smartctl is 6.5 on kernel 4.4 and fails with
`Read NVMe Identify Controller failed` on both drives. `nvme smart-log` works.

## Filesystem: btrfs vs ZFS

| Dimension                      | btrfs                                                                                                                               | ZFS                                        | Weight here                                  |
| ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------ | -------------------------------------------- |
| Kernel                         | In-tree, any kernel including 7.1                                                                                                   | Out-of-tree, LTS 6.18 pin                  | **btrfs.** Also derisks the 5GbE NIC         |
| Mirror integrity               | raid1, checksums, self-heal                                                                                                         | mirror, checksums, self-heal               | Equal                                        |
| Gradual disk failure           | Auto-repair from the good copy                                                                                                      | Auto-repair from the good copy             | Equal, and this is the common case           |
| Sudden total disk loss         | Does not mount, needs manual `-o degraded`                                                                                          | Imports DEGRADED, keeps serving read-write | **ZFS.** The auto-recovery requirement       |
| Snapshots                      | Instant CoW, snapper + `vfs_shadow_copy2`                                                                                           | Instant CoW, `.zfs/snapshot` built in      | Equal, marginally more wiring on btrfs       |
| Per-user quota semantics       | Classic qgroups limit *referenced* space, so deletions free quota immediately. Correct.                                             | `refquota`, live data only                 | Equal on semantics                           |
| Quota + dense snapshots        | qgroup backref computation is global; docs warn of "unacceptable latencies, especially in cases where snapshots scale up"           | Orthogonal, no interaction                 | **ZFS.** 2h/60d is several hundred snapshots |
| Cheaper quota mode             | `squota` (6.7+) avoids the cost but charges deleted data to the owner until snapshots expire; docs target it at *immutable* extents | n/a                                        | Not usable for mutable home data             |
| Per-user quota, shared dataset | Not expressible                                                                                                                     | `userquota@<user>`                         | Not needed                                   |
| Guaranteed minimum space       | Not expressible                                                                                                                     | `reservation`                              | Not needed                                   |
| Encryption                     | LUKS per device                                                                                                                     | Native per dataset                         | Equal                                        |
| Growth path                    | Flexible online device add/replace                                                                                                  | Replace both, `autoexpand`                 | Equal with only 2 bays                       |
| Free space reporting           | `df` misleads, needs `btrfs filesystem usage`                                                                                       | Clear                                      | ZFS, minor                                   |

**ZFS wins two rows, btrfs one.** ZFS takes automatic degraded import and the quota/snapshot
interaction; btrfs takes the kernel.

On the quota row specifically, both btrfs modes have a problem for this workload. Classic qgroups
have the right semantics but `btrfs-quota(8)` warns their global backref computation "can slow down
transaction commits and lead to unacceptable latencies, especially in cases where snapshots scale
up", and 2h over 60 days is several hundred snapshots per subvolume. `squota` avoids that cost but
the same page targets it at *immutable* extents, warning of "awkward scenarios where a subvolume is
empty or deleted but still has significant extents accounted to it". Under squota a user who deletes
100GB waits for snapshot expiry to reclaim their quota, which the Synology does not do.

Mitigations exist (quotas only on a homes filesystem, media without them) but the cost is
unmeasurable until the hardware exists, whereas the ZFS equivalent is a solved problem.

### Choice: ZFS

Taken for automatic degraded import plus quota/snapshot independence, both of which serve the stated
reliability and minimal-maintenance requirements.

The LTS pin is a bounded, predictable cost on a known schedule. The btrfs alternative trades it for
an unbounded one: measuring whether qgroup latency is acceptable on this specific workload, with no
clean fallback if it is not.

Cost: `boot.kernelPackages = pkgs.linuxPackages_6_18`, pinned explicitly rather than following the
moving `pkgs.linuxPackages` default, so a nixpkgs bump cannot shift the kernel under ZFS.

ZFS 2.4.3 declares `kernelMaxSupportedMajorMinor = "7.0"`, but 7.0 is already removed from nixpkgs
as EOL upstream, and 7.1 (what compute runs) is unsupported. Non-LTS kernels EOL faster than ZFS
advances, so the LTS is the only stable target. Revisit at the next LTS, roughly 2 years.

Requires `networking.hostId`.

btrfs raid1 remains a reasonable fallback if the kernel pin ever becomes the binding constraint, but
it would mean accepting either qgroup latency risk or squota's deferred-reclaim semantics.

## Datasets

Datasets are **storage policy** units; shares are **access control** units, and several shares can
live inside one dataset. See [`storage-model.md`](storage-model.md) for why they are separate.

```
tank                       HDD mirror
  tank/users               quota=<total>   bounds everyone's snapshots collectively
    tank/users/<user>      refquota=<n>    per-user, 2h/60d snapshots
  tank/media               recordsize=1M   NO snapshots; movies, tv and downloads all live here
  tank/media/music         snapshots       curated, no *arr imports
  tank/media/books         snapshots
  tank/media/gaming        snapshots
  tank/shared
fast                       NVMe
  fast/appstate            syncthing db, rustic cache
  fast/backup              compute's local backup target
```

**`movies`, `tv` and `downloads` must stay in the single `tank/media` dataset.** Hardlinks cannot
cross datasets (`link()` returns `EXDEV`), so splitting them turns every `*arr` import from an
instant hardlink into a full copy, with transient 2x space on 1.6T. They can still be separate SMB
shares with different permissions, because shares and datasets are independent.

`tank/media` gets **no snapshots**: its precious contents go to B2 daily, and snapshots would pin
every deleted torrent and every quality upgrade. The curated child datasets that no `*arr` writes to
can have them.

Pool properties: `ashift=12`, `compression=lz4`, `atime=off`, `xattr=sa`, `acltype=posixacl`,
`autotrim=on` on `fast`, `encryption=aes-256-gcm` with `keylocation=file://` on NVMe1.

`fast` is mirrored, so a single NVMe failure is survivable rather than merely tolerable.

Use `refquota`, not `quota`, on user datasets. `quota` counts snapshots against the allowance, so
with 2h snapshots a user hits "disk full" for data they deleted weeks ago.

## Users and shares

SMB only in phase 1; NFS exports for machines land later when compute migrates. CIFS gid mount
options are client-local, so laptop's 5190/5512 disagreeing with compute's 5000/5001 needs no
unification now.

Server-side accounts mirror the Synology model: machine accounts (`machine-compute`, `machine-inky`)
writing into shared datasets, plus human accounts owning `tank/home/<user>`. Growth: 2 users now, +1
soon, +1 within a year.

Adding a user is one opt-in flag producing the dataset, quota, share, account, snapshot policy and
backup inclusion. The mechanism is a selfhost-nix producer concern, designed in
[`storage-model.md`](storage-model.md). Removal stays manual: `zfs destroy` must never fire from an
activation script.

### Samba credentials

`syncPasswordsByPam` and unix password sync are dead ends. SMB needs an NT hash, which cannot be
derived from the crypt(3) hash in `hashedPassword`.

Human users get a per-user runtime secret on this host, synced into `passdb.tdb` by an idempotent
oneshot running `smbpasswd -a -s`, following the existing `*-initial-credentials` pattern. The
password never enters the repo; onboarding reads it once via
`ssh <host> sudo cat /var/lib/homelab-secrets/<user>-smb`.

Service accounts differ: the client host needs the same plaintext, so those come from sops encrypted
to both hosts. See [`storage-model.md`](storage-model.md).

This keeps `passdb.tdb` reconstructible: a fresh install rebuilds every account from the declared
secrets instead of restoring unreproducible state.

Samba authenticates against local `tdbsam` with no network dependency, so SMB survives compute being
down.

## Snapshots

`sanoid`, per dataset rather than uniformly. Personal data (`tank/users/*`) keeps the Synology policy:
every 2h, all retained 5 days, dailies retained 60. `tank/media` gets none, and its curated children
get a policy only if they earn one.

`snapdir=visible` plus Samba `vfs_shadow_copy2` gives DSM-style "previous versions" restore.

Snapshots live on the same disks. They cover deletion and corruption, not drive, box or house loss.

## Backup

rustic to Backblaze B2, running here and reading locally instead of over CIFS. No compute
dependency. Compute keeps its own job for local service state (gitea, arr, home-assistant, radicale,
runtime secrets), which cannot move.

Bindings are whole-share; exclusions are data-side in each share's `.gitignore`, which rustic honours
via `git-ignore = true`. That file lives with the data, so it survives the migration unchanged.

**`snapdir=visible` and whole-share backup are a dangerous pair.** The Synology equivalent was
`#snapshot`, 123 directories each holding a full view of a 2.1T share, which the default globs did
not exclude and which would have been walked once per snapshot. ZFS has exactly the same shape: with
`snapdir=visible` set for the Previous Versions UX, `.zfs/snapshot/<name>/` appears in a directory
walk and every snapshot gets traversed. **Add `!.zfs` to the backup globs before enabling
`snapdir=visible` on any share that is backed up.** With the default `snapdir=hidden` the directory
does not appear in `readdir` and a walker will not descend into it, so the hazard only appears when
the browsing UX is switched on.

## Monitoring

node + smartctl exporters scraped by compute's Prometheus, alerting through Alertmanager to ntfy.
node_exporter's ZFS collector covers pool metrics.

Critical alerts also take a direct ntfy path so this host can shout without compute alive:
`services.zfs.zed` fires on pool events (device fault, degraded, scrub error) and runs a zedlet.
Same for SMART failure and backup failure.

**Add a drive temperature alert on `smartctl_device_temperature`**, which compute's scope does not
have (it alerts on health, wear and critical warnings only). The HDDs run 40C with a 47C peak in the
DS923+, a 4-bay chassis with real airflow. Two 7200rpm 12TB helium drives plus three NVMe in a
compact 2-bay enclosure is a worse thermal environment, and Toshiba's ceiling for these is 55C.
Worth having from day one rather than after the first hot summer.

## UPS

The UPS USB moves off the Synology to here, which becomes the NUT server
(`power.ups.mode = "netserver"`). Compute's `power.ups.upsmon.monitor.synology` gets repointed and
renamed.

This inverts today's dependency: compute now relies on this host for graceful shutdown on power
loss. Unavoidable, the cable lives somewhere.

## Maintenance

| Concern             | Mechanism                                                    |
| ------------------- | ------------------------------------------------------------ |
| Bit rot             | `services.zfs.autoScrub` monthly, repairs from the good copy |
| Disk health         | `services.smartd`, short weekly, long monthly                |
| Snapshots + pruning | `sanoid`                                                     |
| SSD longevity       | `services.zfs.trim`                                          |
| Off-site backup     | rustic to B2                                                 |
| Power loss          | NUT server, local                                            |

## Failure modes

| Failure                | Behaviour                                                                                                                   |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| Gradual (bad sectors)  | Read fails checksum, good copy served and block rewritten, scrub sweeps the rest. No intervention. This is the common case. |
| Sudden total disk loss | Pool imports as DEGRADED and keeps serving read-write. `zpool replace` when the disk arrives, resilver is unattended.       |
| Root NVMe death        | Reinstall from the flake. Both pools untouched, re-import.                                                                  |
| `fast` NVMe death      | Mirrored, so survivable; `zpool replace` as with `tank`.                                                                    |

**Rehearse the disk failure before trusting it.** After the build and before the data matters, pull
one HDD, watch the pool go DEGRADED, and `zpool replace` it back. DSM's hand-holding is being traded
for ZFS that has not been run here before, and the sequence is much better learned when the timing is
yours and the contents are disposable.

Drive lifetime, for planning: Backblaze's 2025 fleet AFR is 1.36% and failure rates climb noticeably
past five years of service. At 2.6 years with clean SMART, that puts roughly a 6% chance of one of
the pair failing before year five, and replacement is a year 5-7 conversation. SMART is a weak
predictor though: a meaningful share of failures give no prior warning, so the mirror and B2 are the
protection and SMART only buys warning time when it happens to work.

## Migration

Two phases, because the 12TB pair is in the Synology holding live data and cannot be in both boxes
at once. The NVMe are free (that Synology pool has no volume), so the whole design can be exercised
before anything irreversible happens.

**Phase 1: build on NVMe only.** 1TB bundled as root, the two 512GB as a mirror. **Name that mirror
`tank`, not `fast`**, so the host config is identical to the final one and `disko.nix` is the single
file that changes at cutover. `fast` does not exist yet.

What this proves:

| Proven in phase 1                                                     | Still unknown until phase 2  |
| --------------------------------------------------------------------- | ---------------------------- |
| disko, sops bootstrap, kernel pin, RTL8126A firmware                  | Resilver duration on 12TB    |
| Pool creation, native encryption, keyfile auto-unlock across a reboot | Quota behaviour at real size |
| Snapshots, `snapdir=visible`, `vfs_shadow_copy2`, the `!.zfs` glob    | Thermals with HDDs installed |
| Samba accounts from runtime secrets, exports, permissions             | Restore duration from B2     |
| Mounting from laptop, paths resolving end to end                      |                              |
| Backup job, globs, `.gitignore` handling                              |                              |
| node/smartctl/zed exporters scraped by compute, ntfy alerting         |                              |
| Degraded import and `zpool replace`                                   |                              |

Rehearse the disk failure here rather than later: pull one 512GB, watch the pool go DEGRADED,
resilver it back. Same exercise as on `tank` proper, no consequence.

**Phase 2: cutover.** Shut the Synology down, move the HDDs, point `disko.nix` at them, recreate
`tank`, build `fast` on the 512GB pair, restore from B2 and re-acquire the rest.

**Phase 1 does not reduce the data exposure.** Wiping the 12TB pair still loses the ~2T that is not
in B2 (see open items). "ZFS is proven" and "my data is safe" are separate questions, and only the
first is answered by phase 1.

Do not mount both servers from one client to compare them. `selfhost.storage.mounts.smb` takes a
single `hostname` (`smb.nix:164` builds every device string from it) and `localMount` defaults to
`/mnt/homelab-<share>`, so two servers exporting `media` would collide. For a side-by-side, add a
plain `fileSystems` entry for the duration and delete it after:

```nix
fileSystems."/mnt/storage-media" = {
  device = "//storage/media";
  fsType = "cifs";
  options = [ "credentials=/run/secrets/storage-smb" "x-systemd.automount" "nofail" "noauto"
              "uid=0" "gid=5001" "file_mode=0660" "dir_mode=0770" "vers=default" ];
};
custom.shares.storage-media.root = "/mnt/storage-media";
```

Reusing `gid=5001` avoids a new group and keeps existing service users able to read it. Verified: the
consumer side already handles two storages, since `custom.shares.<n>.root` is just a path. Both roots
coexist, backup correctly excludes the new share, and the opt-in warning names it.

**Why `mounts.smb.servers.<name>` stays unbuilt.** The recurrent multi-NAS pattern is a replication
target (`zfs send | ssh target zfs recv`), which is never mounted by clients and needs nothing from
this option. Only tiering and migration windows want client-side multi-mount, and neither is a shape
worth a third reshape of `storage.mounts`.

## Fleet impact

- `hosts/shared.nix`: entry in `lan.hosts`
- compute: `selfhost.storage.smb.hostname` repointed, `power.ups` monitor repointed, Prometheus
  scrape target added, `NASStorageFull` alert rewritten off `fstype="cifs"`
- inky: fstab fragment repointed
- laptop: `selfhost.storage.smb` repointed
- homepage: `synology-dsm` external tile removed
- dotfiles-private: host entry for sops bootstrap, `.sops.yaml` key
- [`../storage.md`](../storage.md): Synology doc, superseded once this is built

## Open items

01. ~~5GbE NIC on 6.18~~: verified, RTL8126A and i226-V both supported (see Hardware).

02. ~~Hostname~~: `storage`. Rename `lan.hosts.bruno-home-nas` at cutover, or add `storage` alongside during migration.

03. ~~`shared`/`homes`~~: `shared` not created initially, added later as one entry plus one root binding. Synology's `homes` concept is dropped: each user gets a share, nothing more.

04. ~~NVMe capacities and health~~: 1TB bundled (root) + 2x 512GB reused (mirror), both verified healthy at 19% wear. Per-user quota values still open.

05. Whether compute gains `fast/backup` as a second backup target now or later.

06. **Migration data loss is the real exposure, not the filesystem.** 2.5T sits on the HDDs and
    only ~454G is in B2, so wiping them loses roughly 2T of movies, tv, downloads, software and
    comics. There is no in-place migration: the destination drives *are* the source drives.
    Options are accept the re-download, buy new drives, or stage 2T elsewhere first.

07. ~~HDD health~~: both PASSED (2026-08-10). Toshiba HDWG21C 12TB 7200rpm CMR helium, 22,501
    power-on hours (2.6 years), zero reallocated, pending and uncorrectable sectors, zero CRC
    errors, helium condition 100 against a threshold of 75, and 21 clean self-tests. Note plain
    `smartctl -a` reports "device lacks SMART capability" on DSM because it misdetects the
    transport as SCSI; `-d sat` is required.

    Serials are adjacent (`X3H0A04JFP8G`, `X3H0A04AFP8G`) and hours identical, so same model,
    age and batch. For a 2-disk mirror holding everything that is the correlated-failure case;
    B2 plus monthly scrubs and SMART alerting are what stand between it and data loss.

08. ~~Synology NVMe pool contents~~: nothing to preserve. `df` shows only `/volume1`; the NVMe pool
    has no volume, so freeing the drives costs nothing.

09. Add `!.zfs` to the backup globs before enabling `snapdir=visible` (see Backup).

10. ~~Share granularity~~: settled. Kids do not touch SMB, so shares stay coarse and no gid map is
    needed beyond the existing per-share groups.

11. ~~`gaming` split~~: not worth a share. `media/gaming` as a dataset gets snapshots; the backup
    split stays in the existing `.gitignore`, which is already verified working.

Capacity reference from the 2026-08-09 audit, for sizing the pool: media 2.1T (tv 1.2T, movies 417G,
gaming 268G, downloads 102G, music 48G), bphenriques 284G of which photos 265G. Roughly 170G of
media plus 284G of bphenriques is what actually goes off-site.
