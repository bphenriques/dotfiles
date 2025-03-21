{
  lib,
  pkgs,
  resizeArtwork ? true,
  musicIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  artistIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  albumIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/music-artist-symbolic.svg",
  stoppedIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/media-playback-stop-symbolic.svg",
  clearIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/actions/edit-clear-all-symbolic.svg",
  shuffleIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/media-playlist-shuffle-symbolic.svg",
  noShuffleIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/media-playlist-no-shuffle-symbolic.svg",
  repeatSongIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/media-playlist-repeat-song-symbolic.svg",
  noRepeatIcon ? "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark/symbolic/status/media-playlist-no-repeat-symbolic.svg",
  ...
}:
pkgs.writeShellApplication {
  name = "mpc-plus";
  runtimeInputs = [
    pkgs.mpc
    pkgs.libnotify
    #FIXME: re-add fuzzel on next relase
  ] ++ lib.optionals resizeArtwork [
    pkgs.imagemagick
  ];

  text = ''
    MPC_PLUS_SONG_ICON="${musicIcon}"
    MPC_PLUS_ARTIST_ICON="${artistIcon}"
    MPC_PLUS_ALBUM_ICON="${albumIcon}"
    MPC_PLUS_STOPPED_ICON="${stoppedIcon}"
    MPC_PLUS_CLEAR_ICON="${clearIcon}"
    MPC_PLUS_RANDOM_ICON="${shuffleIcon}"
    MPC_PLUS_NO_RANDOM_ICON="${noShuffleIcon}"
    MPC_PLUS_REPEAT_ICON="${repeatSongIcon}"
    MPC_PLUS_NO_REPEAT_ICON="${noRepeatIcon}"

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}