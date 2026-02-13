#!/usr/bin/env bash
# Generates romm secrets if not present. See runbook.md for rotation instructions.

set -euo pipefail
umask 077

SECRETS_FILE="$1"

if [ ! -f "$SECRETS_FILE" ]; then
  DB_PASSWORD=$(openssl rand -base64 32 | tr -d '\n')
  DB_ROOT_PASSWORD=$(openssl rand -base64 32 | tr -d '\n')
  AUTH_SECRET=$(openssl rand -base64 64 | tr -d '\n')
  ADMIN_PASSWORD=$(openssl rand -base64 24 | tr -d '\n')

  cat > "$SECRETS_FILE" <<EOF
DB_PASSWD=$DB_PASSWORD
MYSQL_PASSWORD=$DB_PASSWORD
MYSQL_ROOT_PASSWORD=$DB_ROOT_PASSWORD
ROMM_AUTH_SECRET_KEY=$AUTH_SECRET
ROMM_AUTH_USERNAME=admin
ROMM_AUTH_PASSWORD=$ADMIN_PASSWORD
EOF

  chmod 600 "$SECRETS_FILE"
fi
