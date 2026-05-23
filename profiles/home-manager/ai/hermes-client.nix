# Hermes laptop client — thin OpenAI-compat CLI that calls compute's Hermes API.
#
# Two-step seeding (one-time):
#   1. Rebuild laptop. Activation writes ~/.hermes/config.yaml on first run
#      only (subsequent rebuilds preserve user edits). base_url is filled
#      in from private.settings.services.hermes-api; only api_key remains
#      as a placeholder.
#   2. Copy the bearer token from compute and paste into api_key:
#        ssh compute sudo cat /var/lib/homelab-secrets/hermes-api/api-token
#
# After that, `hermes chat -q "hello"` round-trips: laptop CLI →
# https://hermes-api.<domain>/v1 (resolved to compute's LAN IP via the
# /etc/hosts override in hosts/laptop/default.nix, so traffic stays on LAN
# while HTTPS is preserved by Traefik's wildcard cert) → compute's Hermes
# agent → laptop's Ollama → response.

{ pkgs, inputs, lib, private, ... }:
let
  apiUrl = "${private.settings.services.hermes-api}/v1";
in
{
  home.packages = [ inputs.hermes-agent.packages.${pkgs.system}.default ];

  #TODO: Use yaml generator
  home.activation.hermesClientSeed = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    target="$HOME/.hermes/config.yaml"
    if [ ! -f "$target" ]; then
      mkdir -p "$HOME/.hermes"
      cat > "$target" <<EOF
    # api_key: paste the bearer from compute. See header comment in
    # ~/.dotfiles/profiles/home-manager/ai/hermes-client.nix.
    model:
      provider: custom
      default: hermes-agent
      base_url: ${apiUrl}
      api_key: REPLACE_WITH_TOKEN_FROM_COMPUTE
      context_length: 65536
    EOF
    fi
  '';
}
