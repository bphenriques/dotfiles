{ pkgs, lib }:
(pkgs.writeShellApplication {
  name = "darwin-install";
  runtimeInputs = with pkgs; [ cowsay ];
  text = lib.fileContents ./darwin-install.sh;
}).overrideAttrs(old: {
  buildCommand = "${old.buildCommand}\n patchShebangs $out"; # Ensure we use update the interpreter's location.
})
