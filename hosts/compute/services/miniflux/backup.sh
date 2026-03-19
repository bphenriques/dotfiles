# Exports Miniflux feeds as OPML for backup.
# Recovery: POST /v1/import with the OPML file.
password="$(cat "$MINIFLUX_ADMIN_PASSWORD_FILE")"
rm -rf "$OUTPUT_DIR" && mkdir -p "$OUTPUT_DIR"

curl --fail --silent --show-error --location --config - <<EOF
url = "$MINIFLUX_URL/v1/export"
user = "admin:$password"
output = "$OUTPUT_DIR/feeds.opml"
EOF
