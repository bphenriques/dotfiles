{ lib, pkgs, config, self, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  package = self.packages.mpc-plus.override {
    musicIcon = mkIcon "mpc-plus-track" "";
    artistIcon = mkIcon "mpc-plus-artist" "󰠃";
    albumIcon = mkIcon "mpc-plus-album" "󰀥";
    stoppedIcon = mkIcon "mpc-plus-stopped" "";
    shuffleIcon = mkIcon "mpc-plus-shuffle" "";
    noShuffleIcon = mkIcon "mpc-plus-no-shuffle" "󰒞";
    repeatSongIcon = mkIcon "mpc-plus-repeat-song" "󰑖";
    noRepeatIcon = mkIcon "mpc-plus-no-repeat" "󰑗";
    deviceIcon = mkIcon "mpc-plus-device" "󰓃";
    errorIcon = mkIcon "mpc-plus-error" "";
  };
  mpc-plus = lib.getExe package;

  inherit (config.services.mpd.network) listenAddress port;
in
{
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

  programs.wlr-which-key.settings.menu = lib.mkOrder 100 [
    { key = "m"; desc = "Music"; submenu = [
      { key = "p";            desc = "Play/Pause";        cmd = "${mpc-plus} play-pause";           keep_open = true; }
      { key = "s";            desc = "Stop";              cmd = "${mpc-plus} stop"; }
      { key = ["Left" "h"];   desc = "Previous";          cmd = "${mpc-plus} previous";             keep_open = true; }
      { key = ["Right" "l"];  desc = "Next";              cmd = "${mpc-plus} next";                 keep_open = true; }
      { key = ["Up" "k"];     desc = "Increase volume";   cmd = "${mpc-plus} volume-increase";      keep_open = true; }
      { key = ["Down" "j"];   desc = "Reduce volume";     cmd = "${mpc-plus} volume-decrease";      keep_open = true; }
      { key = "z";            desc = "Toggle repeat";     cmd = "${mpc-plus} toggle-repeat";        keep_open = true; }
      { key = "x";            desc = "Toggle random";     cmd = "${mpc-plus} toggle-random";        keep_open = true; }
      { key = "space";        desc = "Play...";           cmd = "${mpc-plus} dmenu-file-exec play"; }
      { key = "r";            desc = "Stream...";         cmd = "${mpc-plus} dmenu-radio-stream"; }
      { key = "a";            desc = "Shuffle library";   cmd = "${mpc-plus} play-shuffled"; }
      { key = "d";            desc = "Select Server";     cmd = "${mpc-plus} dmenu-select-server"; }
    ]; }
  ];
}
