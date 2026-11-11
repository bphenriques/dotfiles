{ lib, pkgs, config, self, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  glyphs = {
    music = self.lib.glyphs."fa-music";
    shuffle = self.lib.glyphs."fa-shuffle";
    repeat = self.lib.glyphs."md-repeat";
  };

  package = self.packages.mpc-plus.override {
    musicIcon = mkIcon "mpc-plus-track" glyphs.music;
    artistIcon = mkIcon "mpc-plus-artist" self.lib.glyphs."md-account_music";
    albumIcon = mkIcon "mpc-plus-album" self.lib.glyphs."md-album";
    stoppedIcon = mkIcon "mpc-plus-stopped" self.lib.glyphs."fa-stop";
    shuffleIcon = mkIcon "mpc-plus-shuffle" glyphs.shuffle;
    noShuffleIcon = mkIcon "mpc-plus-no-shuffle" self.lib.glyphs."md-shuffle_disabled";
    repeatSongIcon = mkIcon "mpc-plus-repeat-song" glyphs.repeat;
    noRepeatIcon = mkIcon "mpc-plus-no-repeat" self.lib.glyphs."md-repeat_off";
    deviceIcon = mkIcon "mpc-plus-device" self.lib.glyphs."md-speaker";
    errorIcon = mkIcon "mpc-plus-error" self.lib.glyphs."cod-error";
  };
  mpc-plus = lib.getExe package;

  inherit (config.services.mpd.network) listenAddress port;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.mpc package ];

  xdg.configFile."mpc-plus/local.json".text = builtins.toJSON { host = listenAddress; inherit port; };

  # PartOf only propagates stops, so mpd starting must pull the notifier back in.
  systemd.user.services.mpc-plus-local = {
    Install.WantedBy = [ "graphical-session.target" "mpd.service" ];
    Unit = {
      Description = "mpc-plus notifications for local";
      After = [ "mpd.service" ];
      PartOf = [ "mpd.service" ];
    };
    Service = {
      Type = "simple";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [ "MPC_PLUS_SINGLE_DEVICE=local" ];
      ExecStart = "${mpc-plus} notifications-daemon";
    };
  };

  my.programs.desktop-shell.settings.glyphs.music = glyphs;

  my.programs.desktop-shell.menu.entries = lib.mkOrder 700 [
    { key = "m"; desc = "Music"; status = "music"; submenu = [
      { key = "p";            desc = "Play/pause"; icon = self.lib.glyphs."md-play_pause";        cmd = "${mpc-plus} play-pause";           keepOpen = true; }
      { key = "s";            desc = "Stop"; icon = self.lib.glyphs."md-stop";              cmd = "${mpc-plus} stop"; }
      { key = [ "Left" "Right" ]; desc = "Track"; icon = self.lib.glyphs."md-skip_next";           cmd = [ "${mpc-plus} previous" "${mpc-plus} next" ];                   keepOpen = true; }
      { key = [ "Down" "Up" ];    desc = "Volume"; icon = self.lib.glyphs."md-volume_high";          cmd = [ "${mpc-plus} volume-decrease" "${mpc-plus} volume-increase" ]; keepOpen = true; }
      { key = "z";            desc = "Repeat"; icon = glyphs.repeat;            activeWhen = "repeat"; cmd = "${mpc-plus} toggle-repeat";        keepOpen = true; }
      { key = "x";            desc = "Shuffle"; icon = glyphs.shuffle;           activeWhen = "shuffle"; cmd = "${mpc-plus} toggle-random";        keepOpen = true; }
      { key = "space";        desc = "Play file..."; icon = self.lib.glyphs."md-file_music";        cmd = "${mpc-plus} dmenu-file-exec play"; }
      { key = "r";            desc = "Radio..."; icon = self.lib.glyphs."md-radio";            cmd = "${mpc-plus} dmenu-radio-stream"; }
      { key = "a";            desc = "Play all shuffled"; icon = self.lib.glyphs."md-playlist_play"; cmd = "${mpc-plus} play-shuffled"; }
      { key = "d";            desc = "Server..."; icon = self.lib.glyphs."md-server";           cmd = "${mpc-plus} dmenu-select-server"; }
    ]; }
  ];
}
