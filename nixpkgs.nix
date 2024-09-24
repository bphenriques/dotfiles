# Avoids using system's <nixpkgs> channel using legacy nix commands. This way the shell.nix is consistent.
let lock = (builtins.fromJSON (builtins.readFile ./flake.lock)).nodes.nixpkgs.locked;
in import (fetchTarball {
  url = "https://github.com/nixos-unstable/nixpkgs/archive/${lock.rev}.tar.gz";
  sha256 = lock.narHash;
})