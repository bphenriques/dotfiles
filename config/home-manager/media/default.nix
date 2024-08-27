{ pkgs, lib, config, ... }:
{
  home.packages = with pkgs; [
    exiftool
  ] ++ lib.optionals (pkgs.stdenv.isLinux && config.custom.dotfiles.graphicalEnvironment) [
    museeks   # Audio
    vlc       # Video
  ];

  custom.impermanence = {
    g4music = false;
    museeks = true;
    vlc = true;
  };
}
