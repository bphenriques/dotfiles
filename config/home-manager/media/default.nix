{ pkgs, lib, config, headless, ... }:
{
  home.packages = with pkgs; [
    exiftool
  ] ++ lib.optionals (pkgs.stdenv.isLinux && !headless) [
    museeks   # Audio
    vlc       # Video
  ];
}
