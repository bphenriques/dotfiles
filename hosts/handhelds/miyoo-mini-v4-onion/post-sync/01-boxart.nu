# $1 is the mounted card. Runs from this device's directory, so sync.yaml resolves.
# Box art from RomM's export (<platform>/covers/<rom path minus extension>.png beside the library), shrunk
# to Onion's 250x360 limit in each system's Imgs/. Onion looks art up there by file name alone, so games in
# subfolders (modern/) share it.
const MAX_SIZE = "250x360>"

def main [card: string] {
  let roms = open sync.yaml | get roms
  for entry in ($roms.directories | transpose system dir) {
    let target = [$card $roms.root $entry.dir] | path join
    if not ($target | path exists) { continue }
    let imgs = [$target "Imgs"] | path join
    mkdir $imgs

    let covers_dir = [$roms.source $entry.system "covers"] | path join
    let covers = if ($covers_dir | path exists) {
      glob $"($covers_dir)/**/*.png" --no-dir | each {|f| { key: (rom-key $f $covers_dir), path: $f } }
    } else { [] }

    let made = glob $"($target)/**/*" --no-dir --exclude ["Imgs/**" "**/.*"] | each {|rom|
      let cover = $covers | where key == (rom-key $rom $target) | get -o 0.path
      if $cover == null { return null }
      let img = [$imgs $"($rom | path parse | get stem).png"] | path join
      if not ($img | path exists) or ((ls $img).0.modified < (ls $cover).0.modified) {
        # quiet: some covers carry colour profiles libpng warns about; strip drops them, the device ignores them
        ^magick -quiet $cover -strip -resize $MAX_SIZE $"png:($img)"
      }
      $img
    } | compact

    let stale = glob $"($imgs)/*.png" | where {|i| $i not-in $made }
    $stale | each {|i| rm $i }
    print $"  ($entry.dir): ($made | length) images, ($stale | length) removed"
  }
}

# A file's path below `root`, minus its extension: what RomM names a ROM's exported cover after.
def rom-key [file: string, root: string]: nothing -> string {
  let p = $file | path relative-to $root | path parse
  [$p.parent $p.stem] | where {|s| $s != "" } | path join
}
