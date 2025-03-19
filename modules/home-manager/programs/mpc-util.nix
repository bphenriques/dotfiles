{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.mpc-util;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkIcon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; };

  mpc-util = lib.getExe cfg.package;

  actions = [
    { id = "mpc-play-pause";    symbol = "󰐎"; label = "Play/Pause";  exec = cfg.exec.play-pause; }
    { id = "mpc-previous";      symbol = ""; label = "Previous";  exec = cfg.exec.previous; }
    { id = "mpc-next";          symbol = ""; label = "Next";  exec = cfg.exec.next; }
    { id = "mpc-play-stop";    symbol = "󰐎"; label = "Stop";  exec = cfg.exec.stop; }
    { id = "mpc-clear";         symbol = ""; label = "Clear";  exec = cfg.exec.clear; }
    { id = "mpc-shuffle-play";  symbol = ""; label = "Shuffle all songs";  exec = cfg.exec.play-shuffled; }
    { id = "mpc-play-title";    symbol = ""; label = "Play song";  exec = cfg.exec.search-title-play; }
    { id = "mpc-play-artist";   symbol = "󰀖"; label = "Play artist";  exec = cfg.exec.search-artist-play; }
  ];

  dmenu = self.lib.builders.writeDmenuApplication pkgs {
    name = "mpc-util-menu";
    entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) actions;
  };
in
{
  options.custom.programs.mpc-util = {
    enable = lib.mkEnableOption "custom-mpc";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.mpc-util.override {
        playIcon = mkIcon "mpc-util-play" "";
        pauseIcon = mkIcon "mpc-util-pause" "";
        previousIcon = mkIcon "mpc-util-previous" "󰒮";
        nextIcon = mkIcon "mpc-util-next" "󰒭";
        clearIcon = mkIcon "mpc-util-clear" "";
        musicIcon = mkIcon "mpc-util-track" "";
        artistIcon = mkIcon "mpc-util-artist" "󰠃";
        albumIcon = mkIcon "mpc-util-album" "󰀥";
      };
    };

    exec = {
      dmenu               = mkAppOpt (lib.getExe dmenu);
      play-pause          = mkAppOpt ''${mpc-util} play-pause'';
      stop                = mkAppOpt ''${mpc-util} stop'';
      play-shuffled       = mkAppOpt ''${mpc-util} play-shuffled'';
      previous            = mkAppOpt ''${mpc-util} previous'';
      next                = mkAppOpt ''${mpc-util} next'';
      clear               = mkAppOpt ''${mpc-util} clear'';
      search-title-play   = mkAppOpt ''${mpc-util} dmenu-title play'';
      search-artist-play  = mkAppOpt ''${mpc-util} dmenu-artist play'';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.mpc-util" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.mpc
      (pkgs.makeDesktopItem {
        name = "Music";
        desktopName = "Music";
        icon = mkIcon "music-player" "󰥠";
        exec = lib.getExe dmenu;
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            exec = b.exec;
          };
        in listToAttrs (lib.map toAction actions);
      })
    ];

    custom.programs.wlr-which-key.menus.mpc = [
      { key = "p";            desc = "Play/Pause";            cmd = cfg.exec.play-pause;    keep_open = true; }
      { key = ["Left" "h"];   desc = "Previous";              cmd = cfg.exec.previous;      keep_open = true; }
      { key = ["Right" "l"];  desc = "Next";                  cmd = cfg.exec.next;          keep_open = true; }
      { key = "s";            desc = "Stop";                  cmd = cfg.exec.stop;          keep_open = true; }
      { key = "a";            desc = "Shuffle all songs";     cmd = cfg.exec.play-shuffled; keep_open = true; }
      { key = "c";            desc = "Clear";                 cmd = cfg.exec.clear;         keep_open = true; }
      {
        key = "space";
        desc = "Play";
        submenu = [
          { key = "t"; desc = "Title";    cmd = cfg.exec.search-title-play; }
          { key = "a"; desc = "Artist";   cmd = cfg.exec.search-artist-play; }
        ];
      }
    ];
  };
}