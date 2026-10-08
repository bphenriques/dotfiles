# $1 is the mounted card. Runs from this device's directory, so sync.yaml and artwork.xml resolve.
# Generate only: the scrape step and its cache live on compute, so this reads nothing from the network.
#
# Systems worth scraping, and Skyscraper's own platform name where it differs from the library's.
const PLATFORMS = { gb: "gb", gbc: "gbc", gba: "gba", nes: "nes", snes: "snes", megadrive: "megadrive", fbneo: "arcade" }
const FLAGS = "unattend,relative,nohints,skipped,nobrackets"

def main [card: string] {
  let roms = open sync.yaml | get roms
  for system in ($PLATFORMS | columns) {
    let dir = $roms.directories | get -o $system
    if ($dir == null) { continue }
    let path = [$card $roms.root $dir] | path join
    if not ($path | path exists) { continue }

    print $"  ($system) -> ($path)"
    ^Skyscraper -p ($PLATFORMS | get $system) -i $path --flags $FLAGS -a "artwork.xml" -g $path --gamelistfilename "miyoogamelist.xml" -o ([$path "Imgs"] | path join)
  }
}
