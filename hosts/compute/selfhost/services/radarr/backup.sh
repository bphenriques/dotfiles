# Exports Radarr monitored movies as JSON for backup.
# Recovery: use JSON to re-add monitored movies.
api_key="$(cat "$ARR_API_KEY_FILE")"

curl --fail --silent --show-error --location --config - <<EOF
url = "$ARR_URL/api/v3/movie"
header = "X-Api-Key: $api_key"
output = "$OUTPUT_DIR/movies.json"
EOF
