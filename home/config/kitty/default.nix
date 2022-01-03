{ config, lib, pkgs, ... }:

{
  # Installed using Homebrew on MacOS
  # Not using `enable: true` as I manage by my own config.
  home.packages = lib.optionals pkgs.stdenv.isLinux [ pkgs.unstable.kitty ];

  xdg.configFile = {
    "kitty/kitty.conf".source = ./kitty.conf;
    "kitty/doom-one.conf".source = ./doom-one.conf;
  };
}

