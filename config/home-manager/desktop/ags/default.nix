{ pkgs, lib, inputs,  ... }:

# https://github.com/Serpentian/AlfheimOS/blob/master/user/wm/hyprland/ags.nix
{
  home.packages = [
    pkgs.bun
    pkgs.pavucontrol
  ];

  programs.ags = {
    enable = true;
    configDir = ./config; # Init types using ags --init ./config.js
    extraPackages = with pkgs; [
      gtksourceview
      webkitgtk
      accountsservice
    ];
  };
}