{
  lib,
  pkgs,
  musicIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  artistIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  albumIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "mpc-util";
  runtimeInputs = [
    pkgs.mpc
    pkgs.libnotify
    #FIXME: re-add fuzzel on next relase
  ];
  text = ''
    MPC_UTIL_SONG_ICON="${musicIcon}"
    MPC_UTIL_ARTIST_ICON="${artistIcon}"
    MPC_UTIL_ALBUM_ICON="${albumIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}