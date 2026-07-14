#shellcheck shell=bash

if [[ $# -eq 0 ]]; then
  echo "Usage: rom-artwork <platform>:<dir> [<platform>:<dir>...]" >&2
  exit 1
fi

# We are only focused on media, we can scrap the gamelist
gamelist_dir="$(mktemp -d)"
trap 'rm -rf "$gamelist_dir"' EXIT

for pair in "$@"; do
  platform="${pair%%:*}"
  dir="${pair#*:}"

  if [[ ! -d $dir ]]; then
    printf 'skipping %s: %s not found\n' "$platform" "$dir" >&2
    continue
  fi

  printf '=== Scraping %s (%s) ===\n' "$platform" "$dir"
  Skyscraper -p "$platform" -i "$dir" \
    --flags "unattend,onlymissing,nohints" \
    -s screenscraper || printf 'warning: scrape failed for %s\n' "$platform" >&2

  printf '=== Generating artwork for %s ===\n' "$platform"
  Skyscraper -p "$platform" -i "$dir" \
    --flags "unattend" \
    -g "$gamelist_dir" \
    -o "$dir/media" || printf 'warning: generate failed for %s\n' "$platform" >&2
done
