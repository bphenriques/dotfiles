#!/usr/bin/env sh
set -euf

AGE_CONFIG_FOLDER="/persist/config/bphenriques/home/bphenriques/.config/sops/age"
AGE_KEY_FILE="${AGE_CONFIG_FOLDER}/keys.txt"

if [ ! -d "${AGE_CONFIG_FOLDER}" ]; then
  fail "At this point, the folder ${AGE_CONFIG_FOLDER} should have already been mounted by impermanence."
fi

ensure_bitwarden_login
if [ ! -f "$AGE_KEY_FILE" ]; then
  info "Age - Installing private key in ${AGE_KEY_FILE}"
  fetch_bitwarden_private_key 'nix-sops age shared key' > "${AGE_KEY_FILE}"
else
  info "Age - Already present."
fi

