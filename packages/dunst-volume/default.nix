{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "dunst-volume";
  runtimeInputs = with pkgs; [ dunst ponymix ];
  text = lib.fileContents ./src/dunst-volume.sh;
}

# FIXME: use absolute path as the scripts implicitly depend on the home-manager configuration
# FIXME: configurable ICON?