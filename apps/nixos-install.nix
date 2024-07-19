{ pkgs, lib }:
(pkgs.writeShellApplication {
  name = "nixos-install";
  runtimeInputs = with pkgs; [ ];
  text = lib.fileContents ./nixos-install.sh;
}).overrideAttrs(old: {
  buildCommand = "${old.buildCommand}\n patchShebangs $out"; # Ensure we use update the interpreter's location.
})
