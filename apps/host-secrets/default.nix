{ lib, pkgs, self, ... }:
pkgs.writeShellApplication {
  name = "host-secrets";
  # `self`, not `./.`: the latter resolves against the caller's working directory, so the tool only
  # worked from the repo root.
  text = ''
    [ $# -eq 1 ] || { echo "Usage: host-secrets <host>" >&2; exit 1; }
    nix eval --impure --raw --expr "import ${./script.nix} { flake = builtins.getFlake \"${self}\"; host = \"''$1\"; }"
  '';
  meta.description = "Print an example secrets.yaml for a host from its config.sops.secrets";
  meta.platforms = lib.platforms.all;
}
