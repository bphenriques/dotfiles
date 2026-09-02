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
`disko/pool.nix` stops the partition 8 GiB short of the end so a nominally identical drive is never
too small for the vdev; a replacement must keep that reservation. `sgdisk` is installed for this.

## Deferred work

- Encrypt the root NVMe rather than moving the ZFS key to removable USB. A USB key left plugged in has
  the same threat model as today, and unplugged it costs unattended recovery. The box has TPM2 and
  Secure Boot in setup mode, so LUKS with `systemd-cryptenroll --tpm2-device=auto` keeps unattended
  boot. Without measured boot, PCR7 is forgeable, so this protects a pulled disk and not a stolen box,
  which is exactly the RMA and disposal case above.
- Revisit per-user `refquota` only under real capacity pressure.
