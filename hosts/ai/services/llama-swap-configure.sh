# shellcheck shell=bash

# Downloads only what is missing; a present file is trusted, since hashing ~160 GB on every boot is not worth it.
failed=0
while IFS=$'\t' read -r path url sha256; do
  dest="$MODELS_DIR/$path"
  [ -f "$dest" ] && continue
  if [ "$url" = null ]; then
    echo "$path is missing and must be placed by hand" >&2
    failed=1
    continue
  fi
  echo "downloading $path"
  mkdir -p "$(dirname "$dest")"
  # A partial file stays for the next run to resume; a corrupt one does not.
  if ! curl -sfL --retry 5 -C - -o "$dest.part" "$url"; then
    echo "download of $path failed" >&2
    failed=1
    continue
  fi
  if ! echo "$sha256  $dest.part" | sha256sum -c --quiet; then
    rm "$dest.part"
    failed=1
    continue
  fi
  mv "$dest.part" "$dest"
done < <(jq -r '.[] | [.path, (.url // "null"), .sha256] | @tsv' "$MANIFEST")

exit "$failed"
