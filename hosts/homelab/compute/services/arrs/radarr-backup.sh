#!/usr/bin/env bash
# Exports Radarr monitored movies as JSON for backup.
# Recovery: use JSON to re-add monitored movies.
#
# Required env vars:
#   ARR_URL          - Radarr base URL (e.g. http://127.0.0.1:9098)
#   ARR_API_KEY_FILE - Path to file containing API key
#   OUTPUT_DIR       - Directory to write movies.json
set -euo pipefail

api_key="$(cat "$ARR_API_KEY_FILE")"
rm -rf "$OUTPUT_DIR" && mkdir -p "$OUTPUT_DIR"

curl --fail --silent --show-error --location --config - <<EOF
url = "$ARR_URL/api/v3/movie"
header = "X-Api-Key: $api_key"
output = "$OUTPUT_DIR/movies.json"
EOF
