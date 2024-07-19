{ pkgs, lib }:
(pkgs.writeShellApplication {
  name = "dotfiles-install";
  runtimeInputs = with pkgs; [ git ];
  text = lib.fileContents ./dotfiles-install.sh;
}).overrideAttrs(old: {
  buildCommand = "${old.buildCommand}\n patchShebangs $out"; # Ensure we use update the interpreter's location.
})
