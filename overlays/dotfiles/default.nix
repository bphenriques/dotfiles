{ lib, pkgs, ... }:
let
  # Replace the interpreter's location to be one under the nix store.
  patchShebangs = pkg: pkg.overrideAttrs(old: {
    buildCommand = "${old.buildCommand}\n patchShebangs $out";
  });
in
patchShebangs (pkgs.writeShellApplication {
  name = "dotfiles";
  runtimeInputs = with pkgs; [ ];
  text = lib.fileContents ./dotfiles.sh;
})
