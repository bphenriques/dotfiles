{
  lib,
  pkgs,
  playIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  pauseIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  previousIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  nextIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  clearIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  musicIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  artistIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  albumIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  fuzzel ? pkgs.fuzzel,
  ...
}:
pkgs.writeShellApplication {
  name = "mpc-util";
  runtimeInputs = [
    pkgs.mpc
    #fuzzel
  ];
  text = ''
    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}