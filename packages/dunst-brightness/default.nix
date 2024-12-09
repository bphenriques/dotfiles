{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "dunst-brightness";
  runtimeInputs = with pkgs; [ dunst brightnessctl ];
  text = lib.fileContents ./src/dunst-brightness.sh;
}

# FIXME: use absolute path as the scripts implicitly depend on the home-manager configuration
# FIXME: configurable ICON?