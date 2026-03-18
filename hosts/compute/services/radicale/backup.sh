#!/usr/bin/env bash
# Recovery: restore files to /var/lib/radicale/collections
#
# Required env vars:
#   RADICALE_DATA - Path to Radicale collections directory
#   OUTPUT_DIR    - Directory to write backup
set -euo pipefail

rm -rf "$OUTPUT_DIR" && mkdir -p "$OUTPUT_DIR"
cp -a "$RADICALE_DATA/." "$OUTPUT_DIR/"
