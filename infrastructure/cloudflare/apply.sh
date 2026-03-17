#!/usr/bin/env bash
set -euo pipefail

INFRASTRUCTURE_BASE_DIR="${INFRASTRUCTURE_BASE_DIR:-/mnt/homelab-bphenriques/infrastructure}"
SECRETS_FILE="${INFRASTRUCTURE_BASE_DIR}/secrets.yaml"
STATE_FILE="${INFRASTRUCTURE_BASE_DIR}/terraform.tfstate"

cd "$(dirname "$0")"

tf_cmd=( terraform )
case "${1:-}" in
  init) tf_cmd+=( init -backend-config="path=${STATE_FILE}" ); shift ;;
esac
tf_cmd+=( "$@" )

escaped="$(printf '%q ' "${tf_cmd[@]}")"
sops exec-env "$SECRETS_FILE" "$escaped"
