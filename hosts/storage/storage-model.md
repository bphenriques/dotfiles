# Storage model

How storage is modelled across the fleet, and what the future NAS
([`beelink.md`](beelink.md)) has to provide. The consumer side has landed; the producer side needs
hardware.

## Datasets and shares are different units

This is the distinction everything else rests on.

|             | Dataset                                      | Share                                  |
| ----------- | -------------------------------------------- | -------------------------------------- |
| Unit of     | storage policy: snapshots, quota, recordsize | access control: who reads and writes   |
| Granularity | coarse where hardlinks matter                | fine wherever permissions differ       |
| Who sees it | the NAS only                                 | every client, and consumers address it |

**They are independent: several shares can live inside one dataset.** A share is a directory Samba
exports; a dataset is a ZFS policy boundary. That is what lets access control be fine-grained without
subdividing storage.

Two hard constraints pin the boundaries:

**Hardlinks cannot cross datasets.** `link()` returns `EXDEV` between filesystems, and a dataset is a
filesystem. `movies`, `tv` and `downloads` must therefore share one dataset, or every `*arr` import
becomes a full copy instead of an instant hardlink, with transient 2x space on 1.6T of content. They
can still be three separate *shares* with different permissions.

**Access control is why shares subdivide.** Household members include kids, so read and write access
has to differ per category rather than being granted wholesale. That, not storage policy, is what
sets share granularity.

Consequence: `media` is one dataset with **no snapshots**. Its precious contents (music, books,
manga, recipes, `gaming/emulation/*`, `gaming/savegames`) go to B2 daily, and snapshots would
otherwise pin every deleted torrent and every `*arr` quality upgrade. Personal shares get the full
2h/60d policy, because "I deleted it an hour ago" is real there and B2 alone leaves a gap.

## Shares stay coarse

An earlier iteration split `media` into per-category shares so kids could be granted access
selectively. **That is not needed: kids never touch SMB, services surface the content.** The people
who mount shares are the admin, other adults, and machine accounts, and they all want the whole
thing.

| Share          | Dataset        | Access                                            |
| -------------- | -------------- | ------------------------------------------------- |
| `bphenriques`  | `users/<name>` | owner only                                        |
| `<other user>` | `users/<name>` | owner only                                        |
| `media`        | `media`        | adults and service accounts                       |
| `shared`       | `shared`       | not created yet; one entry plus one root when due |

`downloads` needs no share of its own even though it is admin-only. It lives inside `media`, so
directory permissions (`0700`, owned by the admin) exclude other adults who mount the share.
`acltype=posixacl` is in the pool properties for cases like this that group membership handles
badly.

If per-category access is ever wanted, Samba can export a parent and its children independently, so
it stays additive: new exports on the NAS, no change to any client or call site.

## Three layers, only one of which knows the filesystem

| Layer   | Option                      | Filesystem-aware  |
| ------- | --------------------------- | ----------------- |
| consume | `selfhost.storage.mounts.*` | no                |
| serve   | `selfhost.storage.shares.*` | no, takes a path  |
| own     | `selfhost.storage.zfs`      | yes, deliberately |

A share is just a path, so the serving layer never learns whether it is a ZFS dataset, a btrfs
subvolume or a plain directory. Swapping the backing filesystem touches the ownership layer alone.

## Consumer addressing (landed)

```nix
custom.shares.<name>.root      # absolute root, bound per host; null means this host lacks the share
custom.paths.<name> "rel/path" # resolver, readOnly, derived
```

Shaped after `xdg.configHome` plus a resolver. The root is bound by whoever owns the share on this
host: the SMB mount today, a dataset mountpoint on the NAS. Only shares with a bound root appear in
`custom.paths`, so referencing one this host does not mount is an eval error.

```nix
path         = config.custom.paths.media "movies";
musicLibrary = osConfig.custom.paths.media "music/library";
locations    = [ (paths.media "movies") ];              # parentheses required in list context
folder       = mkSyncFolder "music" (pathsCfg.media "music/library") devices;
```

**The parentheses are a real footgun.** `[ paths.media "movies" ]` is a two-element list, and
`f "a" paths.media "b"` binds four arguments. Both fail on type in practice, but only because the
surrounding options happen to be strict.

## Users and service accounts

Human users and machine identities are separate concepts. Core owns a minimal `serviceAccounts`; the
storage module extends both, following the `services/filebrowser/selfhost.nix` precedent where a
module owns its own per-user surface rather than growing core's schema.

```nix
selfhost.users.alice.storage = { enable = true; quota = "1T"; shares.media = "rw"; };
selfhost.serviceAccounts.machine-compute.storage.shares = { media = "rw"; bphenriques = "rw"; };
selfhost.serviceAccounts.machine-inky.storage.shares.media = "ro";
```

Both sides can be *granted* shares; only `users` can *own* one. Grants are direct rather than via
`selfhost.groups`, so machine identities never enter the human group model.

### Credentials

Compute authenticates over SMB from `sops.secrets."homelab/samba/*"`, so producer and client must
agree on the same plaintext. Runtime secrets cannot express that.

| Account type    | Who needs the plaintext                  | Mechanism                               |
| --------------- | ---------------------------------------- | --------------------------------------- |
| Human user      | Only the NAS; person is told out of band | Runtime secret generated on the NAS     |
| Service account | The NAS **and** the client host          | sops, encrypted to both hosts' age keys |

Each machine still gets its own distinct credential: one logical secret with two recipients, not two
services sharing one.

### Opt-out

`enable = false` removes the share and the account and leaves the dataset untouched. Nix cannot see
the disk at eval time, so a reconcile unit compares declared against actual and warns about orphans
with the exact `zfs destroy` to run by hand. Automatic destruction on a config change means one typo
silently deletes a family member's data; this is the one thing that stays manual.

## Backup (landed)

Bindings are **whole-share**, so a new folder is protected by default. Exclusions are data-side in
each share's `.gitignore`, which rustic honours via `git-ignore = true`. That file lives with the
data, so it survives the migration unchanged.

```nix
bindings = {
  "/system/homelab-secrets" = config.selfhost.runtimeSecretsDir;
  "/nas/bphenriques"        = shares.bphenriques.root;
  "/nas/media"              = shares.media.root;
};
```

Repo paths are invariant between narrow and whole-share bindings, so existing snapshots stayed
restore-compatible and the first run after the switch was a rescan, not a re-upload.

Synology's `#snapshot` and `#recycle` are excluded by **globs**, not `.gitignore`: `#snapshot` is
read-only so a marker cannot be placed in it, and infrastructure exclusions should not be losable
with a file. `.gitignore` stays pure content policy.

## Status

### Landed

| Work                                                            | Verification                                                                                    |
| --------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `selfhost.domain` moved to `selfhost.ingress.domain`            | `nix flake check`; a host with no routed service evaluates with no domain                       |
| `selfhost.storage.smb` renamed to `selfhost.storage.mounts.smb` | CIFS mounts and automount guards unchanged on compute and laptop                                |
| `!#snapshot` / `!#recycle` in framework default globs           | rustic 0.11.3 on a synthetic tree: excluded with globs, included without                        |
| `/mnt/homelab-media/.gitignore`                                 | rustic run against a tree mirroring real top-level names, using the actual file                 |
| Whole-share backup bindings (11 entries to 3)                   | First real run added ~1.4G against a ~1.5G estimate                                             |
| `custom.shares` + `custom.paths` resolver                       | Laptop's system derivation byte-identical to the pre-refactor baseline; resolved values checked |

selfhost-nix at `fb59a7a`; dotfiles builds without an input override.

### Pending

| Work                                                                | State                                 |
| ------------------------------------------------------------------- | ------------------------------------- |
| Per-category human shares for access control                        | Producer-side only; needs the gid map |
| `hosts/storage/datasets.nix`, ZFS settings keyed by share           | Producer, needs hardware              |
| `selfhost.storage.shares.smb` (serving), `selfhost.storage.zfs`     | Producer, needs hardware              |
| Per-user storage opt-in, reconcile unit, `selfhost.serviceAccounts` | Producer, needs hardware              |

Nothing pending touches compute, laptop or the ~28 call sites. The human shares are additional Samba
exports on the NAS; the service tier keeps mounting `media` as it does today.

### Worth doing next time you touch the backup

`sudo rustic -P backblaze ls latest` on compute, to confirm the newly protected files are in the
snapshot. The 1.4G delta matches the estimate but does not prove which files.

## Filesystem audit, 2026-08-09

Paths that existed but were never declared: `podcasts`, `software`, `books/inbox`, `comics/inbox`,
`manga/inbox`, `recipes/{inbox,library}`, `downloads/{complete,radarr,sonarr,torrents}`,
`gaming/{installers,ready,savegames,eXoWin3x,eXoWin9x}`, `gaming/emulation/{saves,dats}`, `devices`,
`documents/library`.

Newly protected by the whole-share switch: loose files at the `bphenriques` root (including the Home
Assistant backup emergency kit), `devices`, `gaming/emulation/saves`, `gaming/savegames`, `recipes`,
`books`, `manga`, `podcasts`.

`notes/.gitignore` already existed and already shapes backups, excluding Obsidian UI churn only.

Sizes: media 2.1T (tv 1.2T, movies 417G, gaming 268G of which roms 120G, downloads 102G, music 48G),
bphenriques 284G of which photos 265G, `#recycle` 109G, `#snapshot` 123 directories.

## Open

- The gid map for the split shares, and which groups kids versus adults land in.
- Whether `gaming` splits further: `emulation/*` and `savegames` are backed up, `installers`, `ready`
  and `eXoWin*` are not, so it keeps a `.gitignore` regardless of how shares are sliced.
- Whether `shared` survives at all. Mounted at gid 5002, no declared paths, not actively in use.
