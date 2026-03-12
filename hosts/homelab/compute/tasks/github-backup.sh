#!/usr/bin/env bash
# Downloads GitHub repository tarballs for backup.
# Recovery: extract tarballs.
#
# Required env vars:
#   GITHUB_TOKEN_FILE - Path to file containing GitHub token
#   GITHUB_REPOS      - Space-separated list of owner/repo
#   OUTPUT_DIR        - Directory to write tarballs
set -euo pipefail

gh_token="$(cat "$GITHUB_TOKEN_FILE")"
rm -rf "$OUTPUT_DIR" && mkdir -p "$OUTPUT_DIR"

for repo in $GITHUB_REPOS; do
  safe_name="${repo//\//_}"
  echo "Downloading $repo..."
  curl --fail --silent --show-error --location --config - <<EOF
url = "https://api.github.com/repos/$repo/tarball"
header = "Authorization: Bearer $gh_token"
header = "Accept: application/vnd.github+json"
output = "$OUTPUT_DIR/$safe_name.tar.gz"
EOF
done
