{ lib, pkgs, ... }:

with lib;
pkgs.writeShellApplication {
  name = "preview";
  runtimeInputs = with pkgs; [
    file        # See type of file
    bat         # Preview files
    tree        # Preview directories
    chafa       # Preview images
    jq          # Preview JSON
    yq          # Preview YAML
    unzip       # Preview zip contents
    imagemagick # Preview PDFs
    ghostscript # Preview PDFs
  ];
  text = fileContents ./preview.sh;
}
