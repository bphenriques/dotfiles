#shellcheck shell=bash
# ARTWORK_XML is set by default.nix (runtimeEnv)

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

  # ScreenScraper matches by checksum or known file name, so custom builds (e.g. Win9x packages)
  # come back empty and some matches have no cover. TheGamesDB searches by title: retry those.
  reports="$HOME/.skyscraper/reports"
  rm -f "$reports/report-$platform-missing_cover-"*.txt
  Skyscraper -p "$platform" -i "$dir" --cache report:missing=cover >/dev/null || true
  report=
  for f in "$reports/report-$platform-missing_cover-"*.txt; do report=$f; done
  if [[ -s $report ]]; then
    printf '=== Retrying games without a cover from TheGamesDB ===\n'
    Skyscraper -p "$platform" -i "$dir" \
      --flags "unattend,nohints" \
      --includefrom "$report" \
      -s thegamesdb || printf 'warning: thegamesdb scrape failed for %s\n' "$platform" >&2
  fi

  printf '=== Generating artwork for %s ===\n' "$platform"
  Skyscraper -p "$platform" -i "$dir" \
    --flags "unattend" \
    -a "$ARTWORK_XML" \
    -g "$gamelist_dir" \
    -o "$dir/media" || printf 'warning: generate failed for %s\n' "$platform" >&2
done
