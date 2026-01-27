#shellcheck shell=bash

set -euo pipefail

CLIENT_ID=$(openssl rand -hex 16)
CLIENT_SECRET=$(openssl rand -base64 32 | tr -d '/+=' | head -c 48)

echo "OAUTH2_CLIENT_ID=${CLIENT_ID}"
echo "OAUTH2_CLIENT_SECRET=${CLIENT_SECRET}"
