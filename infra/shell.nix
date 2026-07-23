{ pkgs }:
let
  tofu = pkgs.opentofu.withPlugins (p: [ p.cloudflare_cloudflare ]);

  # `tofu` wrapped so secrets never enter the shell env.
  tofuWrapper = pkgs.writeShellApplication {
    name = "tofu";
    runtimeInputs = [ pkgs.sops pkgs.git ];
    text = ''
      TOFU=${tofu}/bin/tofu
      ${builtins.readFile ./tofu.sh}
    '';
  };
in
pkgs.mkShellNoCC {
  name = "infra";
  meta.description = "Manage the fleet's Cloudflare infrastructure (OpenTofu)";
  packages = [ tofuWrapper pkgs.sops pkgs.curl pkgs.jq ];

  shellHook = ''
    cd "$(git rev-parse --show-toplevel)/infra" || true
    echo "infra devShell: 'tofu' injects secrets + tfvars. Run: tofu init && tofu plan"
  '';
}
