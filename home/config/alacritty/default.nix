{ config, lib, pkgs, ... }:

{
  # Installed using Homebrew on MacOS
  # Not using `enable: true` as I manage by my own config.
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.unstable.alacritty ];

  xdg.configFile = {
    "alacritty/alacritty.yml".source = ./alacritty.yml;
  };
}

