# Rebuild the site only when the library changed, then swap it in: darkhttpd resolves every request against the site
# path, so repointing the `site` symlink switches builds atomically, without a restart or an empty moment.
library="$1"
state="$2"

fingerprint() {
  find "$library" -type f ! -name '.*' -printf '%P %s %T@\n' | sort | sha256sum
}

current=$(fingerprint)
if [ -L "$state/site" ] && [ "$current" = "$(cat "$state/fingerprint" 2>/dev/null || true)" ]; then
  exit 0
fi

# A write during the build may not re-trigger the path unit, so build again until the library holds still.
while true; do
  mkdir -p "$state/builds"
  build=$(mktemp -d "$state/builds/XXXXXX")
  cook build web --base-path "$library" "$build"
  ln -sfn "$build" "$state/site.new"
  [ -L "$state/site" ] || rm -rf "$state/site"   # the first run replaces the plain directory older builds left
  mv -T "$state/site.new" "$state/site"
  find "$state/builds" -mindepth 1 -maxdepth 1 ! -path "$build" -exec rm -rf {} +
  echo "$current" > "$state/fingerprint"
  after=$(fingerprint)
  [ "$after" = "$current" ] && break
  current=$after
done
