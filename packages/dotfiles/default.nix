{ lib, pkgs, ... }:
let
  # Replace the interpreter's location to be one under the nix store.
  patchShebangs = pkg: pkg.overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
patchShebangs (pkgs.writeScriptBin "dotfiles" (lib.fileContents ./dotfiles.sh))
