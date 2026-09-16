# shellcheck shell=bash
# Mints a Setup URI for enrolling a new Obsidian device against the running daemon.
#
# Printed once and never written to disk: the URI carries the CouchDB credentials and the vault
# encryption passphrase. Pass a passphrase as the first argument to seal it with your own, otherwise
# one is generated and printed alongside, which is the same channel. Separate them if that matters.

if [ "$(id -u)" != 0 ]; then
  echo "must run as root: the CouchDB password and the vault passphrase are root-only" >&2
  exit 1
fi

hostname="$LIVESYNC_COUCHDB_URI"
database="$LIVESYNC_COUCHDB_DATABASE"
username="$LIVESYNC_COUCHDB_USER"
password="$(cat "$LIVESYNC_COUCHDB_PASSWORD_FILE")"
passphrase="$(cat "$LIVESYNC_PASSPHRASE_FILE")"
export hostname database username password passphrase

if [ $# -gt 0 ]; then
  uri_passphrase="$1"
  export uri_passphrase
fi

# Pinned to the daemon's own version, so the URI's defaults and the settings it will replicate
# against are cut from the same release.
exec deno run --allow-env \
  "https://raw.githubusercontent.com/vrtmrz/obsidian-livesync/${LIVESYNC_VERSION}/utils/setup/generate_setup_uri.ts"
