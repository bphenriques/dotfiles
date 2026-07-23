# Injects the relevant secrets in runtime

: "${DOTFILES_PRIVATE:=$HOME/.dotfiles-private}"
secrets="$DOTFILES_PRIVATE/infra/secrets.yaml"
state="$DOTFILES_PRIVATE/infra/terraform.tfstate"

cd "$(git rev-parse --show-toplevel)/infra"
nix eval --json --file "$DOTFILES_PRIVATE/infra/settings.nix" >cloudflare.auto.tfvars.json

encryption() {
  printf 'key_provider "pbkdf2" "state" { passphrase = "%s" }\nmethod "aes_gcm" "state" { keys = key_provider.pbkdf2.state }\nstate { method = method.aes_gcm.state }\n' \
    "$(sops -d --extract '["cloudflare"]["state_passphrase"]' "$secrets")"
}

CLOUDFLARE_API_TOKEN="$(sops -d --extract '["cloudflare"]["api_token"]' "$secrets")" \
TF_ENCRYPTION="$(encryption)" \
TF_CLI_ARGS_init="-backend-config=path=$state" \
  "$TOFU" "$@"
