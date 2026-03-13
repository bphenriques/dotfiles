#!/usr/bin/env bash
# Exports Miniflux feeds as OPML for backup.
# Recovery: POST /v1/import with the OPML file.
#
# Required env vars:
#   MINIFLUX_URL              - Miniflux base URL (e.g. http://127.0.0.1:8081)
#   MINIFLUX_ADMIN_PASSWORD_FILE - Path to file containing admin password
#   OUTPUT_DIR                - Directory to write feeds.opml
set -euo pipefail

password="$(cat "$MINIFLUX_ADMIN_PASSWORD_FILE")"
rm -rf "$OUTPUT_DIR" && mkdir -p "$OUTPUT_DIR"

curl --fail --silent --show-error --location --config - <<EOF
url = "$MINIFLUX_URL/v1/export"
user = "admin:$password"
output = "$OUTPUT_DIR/feeds.opml"
EOF
