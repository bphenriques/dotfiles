{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "preview";
  runtimeInputs = [
    pkgs.file        # See type of file
    pkgs.bat         # Preview files
    pkgs.tree        # Preview directories
    pkgs.chafa       # Preview images
    pkgs.jq          # Preview JSON
    pkgs.yq          # Preview YAML
    pkgs.unzip       # Preview zip contents
    pkgs.imagemagick # Preview PDFs
    pkgs.ghostscript # Preview PDFs
  ];
  text = lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.all;
}
