{ pkgs }:
let
  tofu = pkgs.opentofu.withPlugins (p: [ p.cloudflare_cloudflare ]);
in
pkgs.mkShellNoCC {
  name = "infra";
  meta.description = "Manage the fleet's Cloudflare infrastructure (OpenTofu)";
  packages = [ tofu pkgs.sops pkgs.curl pkgs.jq ];

  shellHook = ''
    : "''${DOTFILES_PRIVATE:=$HOME/.dotfiles-private}"
    export DOTFILES_PRIVATE
    cf_settings="$DOTFILES_PRIVATE/infra/settings.nix"
    cf_secrets="$DOTFILES_PRIVATE/infra/secrets.yaml"
    cf_state="$DOTFILES_PRIVATE/infra/terraform.tfstate"

    cd "$(git rev-parse --show-toplevel)/infra" || return

    # Non-secret config (domain, zone id, records) lives plaintext in the private repo; read at
    # runtime so a record edit needs no flake-lock bump, and the public repo stays data-free.
    if [ -f "$cf_settings" ]; then
      nix eval --json --file "$cf_settings" > cloudflare.auto.tfvars.json
    else
      echo "warning: $cf_settings not found — set DOTFILES_PRIVATE" >&2
    fi

    # Encrypted state lives in dotfiles-private; wire its path into `tofu init`.
    export TF_CLI_ARGS_init="-backend-config=path=$cf_state"

    # Token + state passphrase are decrypted per `tofu` call, never exported to the shell env.
    _cf_encryption() {
      printf 'key_provider "pbkdf2" "state" { passphrase = "%s" }\nmethod "aes_gcm" "state" { keys = key_provider.pbkdf2.state }\nstate { method = method.aes_gcm.state }\n' \
        "$(sops -d --extract '["cloudflare"]["state_passphrase"]' "$cf_secrets")"
    }
    tofu() {
      CLOUDFLARE_API_TOKEN="$(sops -d --extract '["cloudflare"]["api_token"]' "$cf_secrets")" \
        TF_ENCRYPTION="$(_cf_encryption)" \
        command tofu "$@"
    }

    echo "infra devShell ready (cwd: infra). Run: tofu init && tofu plan"
  '';
}
