#shellcheck shell=bash

if [[ $# -eq 0 ]]; then
  echo "Usage: scrape-roms <roms-dir> [platform...]"
  echo "  If no platforms given, scans subdirectories."
  exit 1
fi

ROMS_ROOT="$1"; shift

[[ -d $ROMS_ROOT ]] || {
  printf 'ROM root not found: %s\n' "$ROMS_ROOT" >&2
  exit 1
}

if [[ $# -gt 0 ]]; then
  platforms=("$@")
else
  platforms=()
  for dir in "$ROMS_ROOT"/*/; do
    [[ -d $dir ]] || continue
    platforms+=("$(basename "$dir")")
  done
fi

for platform in "${platforms[@]}"; do
  roms_dir="$ROMS_ROOT/$platform"
  [[ -d $roms_dir ]] || continue

  printf '=== Scraping %s ===\n' "$platform"
  Skyscraper -p "$platform" -i "$roms_dir" \
    --flags "unattend,onlymissing,nohints" \
    -s screenscraper || printf 'Warning: scraping failed for %s, skipping\n' "$platform" >&2
done
