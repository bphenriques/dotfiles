# Push a handheld's hosts/handhelds/<device>/sync.toml onto it, over ssh or through its mounted card.
# rsync is idempotent either way, so re-running costs nothing.
#
#   handheld-sync hosts/handhelds/rgsp-spruce          --ssh 192.168.1.50
#   handheld-sync hosts/handhelds/miyoo-mini-v4-onion  --card /run/media/you/CARD

# Syncthing's leftovers and editor noise: never wanted on a device.
const NEVER_PUSH = [ ".stfolder" ".stignore" ".idea" ]

# Mirrors fleet.roms.excluded. Scoped to the ROM directories, or it would also drop the README.md files
# the overlay trees legitimately carry.
const ROMS_EXCLUDE = [ "media" "patches" "archive" "README.md" "systeminfo.txt" ]

def read-config [dir: string]: nothing -> record {
  let file = [$dir "sync.yaml"] | path join
  if not ($file | path exists) { error make --unspanned { msg: $"no such file: ($file)" } }
  let cfg = open $file

  let roms = $cfg | get -o roms
  let roms_paths = if ($roms | is-empty) { [] } else {
    let root = $roms | get -o root | default ""
    let source = $roms | get -o source | default ""
    if ($root | is-empty) or ($source | is-empty) {
      error make --unspanned { msg: $"($file): roms needs both source and root" }
    }
    # Mirrored, unlike everything else: a per-system ROM directory is ours alone, so what the library
    # dropped should leave the card too.
    $roms | get -o directories | default {} | items {|system, dir|
      { source: ([$source $system] | path join), dest: ([$root $dir] | path join), mirror: true }
    }
  }

  let extra = $cfg | get -o sync | default [] | each {|p|
    if (($p | get -o source | default "") | is-empty) or (($p | get -o dest | default "") | is-empty) {
      error make --unspanned { msg: $"($file): every [[sync]] needs both source and dest" }
    }
    { source: $p.source, dest: $p.dest, mirror: false }
  }

  let paths = $roms_paths | append $extra
  if ($paths | is-empty) { error make --unspanned { msg: $"($file): nothing to sync" } }

  let missing = $paths | get source | where {|s| not ($s | path exists) }
  if ($missing | is-not-empty) {
    error make --unspanned { msg: $"source not readable from here: ($missing | str join ', ')" }
  }

  {
    file: $file
    cardRoot: ($cfg | get -o cardRoot | default "")
    ssh: ($cfg | get -o ssh | default { })
    paths: $paths
    hooks: (glob ([$dir "post-sync" "*.nu"] | path join) | sort)
  }
}

def main [
  device: string            # the device directory, e.g. hosts/handhelds/rgsp-spruce
  --ssh: string = ""        # reach it over ssh at this address
  --card: string = ""       # or through its card, mounted at this path
  --apply                   # without it rsync only reports what it would do
] {
  if ($ssh | is-empty) == ($card | is-empty) {
    error make --unspanned { msg: "give exactly one of --ssh <address> / --card <mount>" }
  }
  if ($card | is-not-empty) and not ($card | path exists) {
    error make --unspanned { msg: $"no such mount: ($card)" }
  }
  let card = if ($card | is-empty) { "" } else { $card | path expand }  # hooks run elsewhere, so absolute
  let cfg = read-config $device

  # Destinations are relative to the card, so the mount replaces the device's own view of it.
  let root = if ($card | is-not-empty) { $card } else { $cfg.cardRoot }
  if ($root | is-empty) {
    error make --unspanned { msg: $"($cfg.file): reaching this device over ssh needs cardRoot" }
  }
  let user = $cfg.ssh | get -o user | default "root"
  let remote_rsync = $cfg.ssh | get -o rsync | default ""

  for p in $cfg.paths {
    let source = ($p.source | str trim --right --char "/") + "/"  # trailing slash: contents, not the dir
    let target = [$root $p.dest] | path join
    let dest = if ($card | is-empty) { $"($user)@($ssh):($target)/" } else { $target + "/" }
    print $"(ansi cyan)($source) -> ($dest)(ansi reset)"

    # No owner/group/mode: FAT32 rejects all three with EPERM, which exits 23 and leaves every file
    # looking different next run. Times stay, or rsync loses its quick check and re-sends everything.
    let flags = [--archive --no-owner --no-group --no-perms --no-links --human-readable --itemize-changes --mkpath]
      | append ($NEVER_PUSH | append (if $p.mirror { $ROMS_EXCLUDE } else { [] }) | each {|n| $"--exclude=($n)" })
      | append (if $p.mirror { [--delete] } else { [] })
      | append (if $apply { [] } else { [--dry-run] })
      | append (if ($card | is-not-empty) or ($remote_rsync | is-empty) { [] } else { [$"--rsync-path=($remote_rsync)"] })

    ^rsync ...$flags $source $dest
  }

  # post-sync/*.nu in name order, run from the device directory with the card as the first argument, so a
  # hook can read its own sync.toml. Local rather than on the device: these adapt content for this
  # handheld, and the tooling for that (Skyscraper, artwork.xml) lives here.
  if ($cfg.hooks | is-not-empty) {
    if ($card | is-empty) {
      print $"\n(ansi yellow)skipping ($cfg.hooks | length) post-sync script\(s\): they need --card, not --ssh(ansi reset)"
    } else if not $apply {
      print $"\n(ansi cyan)would then run: ($cfg.hooks | path basename | str join ', ')(ansi reset)"
    } else {
      cd $device
      for h in $cfg.hooks {
        print $"\n(ansi cyan)running ($h | path basename)(ansi reset)"
        ^nu $h $card
      }
    }
  }

  if not $apply { print $"\n(ansi cyan)dry run; re-run with --apply(ansi reset)" }
}
