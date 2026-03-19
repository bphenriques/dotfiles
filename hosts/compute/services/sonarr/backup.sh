# Exports Sonarr monitored TV series as JSON for backup.
# Recovery: use JSON to re-add monitored TV shows.
api_key="$(cat "$ARR_API_KEY_FILE")"
rm -rf "$OUTPUT_DIR" && mkdir -p "$OUTPUT_DIR"

curl --fail --silent --show-error --location --config - <<EOF
url = "$ARR_URL/api/v3/series"
header = "X-Api-Key: $api_key"
output = "$OUTPUT_DIR/series.json"
EOF
