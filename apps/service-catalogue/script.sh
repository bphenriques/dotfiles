# shellcheck shell=bash
set -euo pipefail

usage() {
  echo "Usage: service-catalogue <host>"
  exit 1
}

[[ $# -eq 1 ]] || usage

host="$1"
echo "> **Auto-generated** from the NixOS service registry (\`nix run .#service-catalogue -- ${host}\`). Do not edit manually."
echo ""
nix eval --impure --json --expr "
  let
    flake = builtins.getFlake (toString ./.);
    nixos = flake.nixosConfigurations.${host};
  in nixos.config.custom.homelab.catalogue
" | jq -r '
  def auth_str:
    if .auth.oidc and .auth.forwardAuth then "OIDC + ForwardAuth"
    elif .auth.oidc then "OIDC"
    elif .auth.forwardAuth then "ForwardAuth"
    else "—"
    end;

  def header:
    "| Name | Description | Version | Subdomain | Internal Port | Auth | Scope |",
    "|------|-------------|---------|-----------|---------------|------|-------|";

  def sanitize: tostring | gsub("\\|"; "\\\\|");

  def row:
    "| [\(.displayName | sanitize)](\(.homepage)) | \(.description | sanitize) | \(.version | sanitize) | `\(.subdomain)` | \(.port) | \(auth_str) | \(.scope) |";

  ["General", "Media", "Monitoring", "Administration"] as $order |
  ($order | to_entries | map({(.value): .key}) | add // {}) as $idx |
  [to_entries[] | .value] | sort_by(.category, .name) | group_by(.category) |
  sort_by(($idx[.[0].category] // 999))[] |
  "### \(.[0].category)", "", header, (.[] | row), ""
'
