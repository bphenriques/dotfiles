# shellcheck shell=bash
set -euo pipefail

usage() {
  echo "Usage: host-secrets <host>"
  exit 1
}

[[ $# -eq 1 ]] || usage

host="$1"
nix eval --impure --raw --expr "
  let
    flake = builtins.getFlake (toString ./.);
    lib = flake.inputs.nixpkgs.lib;
    nixos = flake.nixosConfigurations.${host};
  in
    builtins.readFile (
      (nixos.pkgs.formats.yaml {}).generate \"host-secrets-example.yaml\" (
        builtins.foldl'
          (acc: key:
            lib.recursiveUpdate
              acc
              (lib.attrsets.setAttrByPath (lib.splitString \"/\" key) \"secret\")
          )
          {}
          (builtins.attrNames nixos.config.sops.secrets)
      )
    )
"
