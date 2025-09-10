{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.mpc-plus;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  mpc-plus = lib.getExe cfg.package;
in
{
  options.custom.programs.mpc-plus = {
    enable = lib.mkEnableOption "custom-mpc";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.mpc-plus.override {
        musicIcon = mkIcon "mpc-plus-track" "";
        artistIcon = mkIcon "mpc-plus-artist" "󰠃";
        albumIcon = mkIcon "mpc-plus-album" "󰀥";
        stoppedIcon = mkIcon "mpc-plus-stopped" "";
        clearIcon = mkIcon "mpc-plus-clear" "";
        shuffleIcon = mkIcon "mpc-plus-shuffle" "";
        noShuffleIcon = mkIcon "mpc-plus-no-shuffle" "󰒞";
        repeatSongIcon = mkIcon "mpc-plus-repeat-song" "󰑖";
        noRepeatIcon = mkIcon "mpc-plus-no-repeat" "󰑗";
      };
    };

    exec = {
      play-pause        = mkAppOpt ''${mpc-plus} play-pause'';
      stop              = mkAppOpt ''${mpc-plus} stop'';
      play-shuffled     = mkAppOpt ''${mpc-plus} play-shuffled'';
      previous          = mkAppOpt ''${mpc-plus} previous'';
      next              = mkAppOpt ''${mpc-plus} next'';
      clear             = mkAppOpt ''${mpc-plus} clear'';
      toggle-random     = mkAppOpt ''${mpc-plus} toggle-random'';
      toggle-repeat     = mkAppOpt ''${mpc-plus} toggle-repeat'';
      volume-increase   = mkAppOpt ''${mpc-plus} volume-increase'';
      volume-decrease   = mkAppOpt ''${mpc-plus} volume-decrease'';
      search-play       = mkAppOpt ''${mpc-plus} dmenu-file-exec play'';
      search-enqueue    = mkAppOpt ''${mpc-plus} dmenu-file-exec add'';
      search-next       = mkAppOpt ''${mpc-plus} dmenu-file-exec next'';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.mpc-plus" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.mpc
      cfg.package
      (pkgs.makeDesktopItem {
        name = "Music";
        desktopName = "Music";
        icon = mkIcon "music-player" "󰥠";
        exec = "${lib.getExe config.custom.programs.wlr-which-key.package} mpc-plus";
        actions = {
          "shuffle"   = { name = "Shuffle library"; icon = (mkIcon "mpc-plus-shuffle-library" ""); exec = cfg.exec.play-shuffled; };
          "find-play" = { name = "Play...";         icon = (mkIcon "mpc-plus-find-play" "");       exec = cfg.exec.search-play; };
          "stop"      = { name = "Stop";            icon = (mkIcon "mpc-plus-stop" "");            exec = cfg.exec.stop; };
        };
      })
    ];

    custom.programs.wlr-which-key.menus.mpc-plus = [
      { key = "p";            desc = "Play/Pause";        cmd = cfg.exec.play-pause;      keep_open = true; }
      { key = "s";            desc = "Stop";              cmd = cfg.exec.stop; }
      { key = ["Left" "h"];   desc = "Previous";          cmd = cfg.exec.previous;        keep_open = true; }
      { key = ["Right" "l"];  desc = "Next";              cmd = cfg.exec.next;            keep_open = true; }
      { key = ["Up" "k"];     desc = "Increase volume";   cmd = cfg.exec.volume-increase; keep_open = true; }
      { key = ["Down" "j"];   desc = "Reduce volume";     cmd = cfg.exec.volume-decrease; keep_open = true; }
      { key = "z";            desc = "Toggle repeat";     cmd = cfg.exec.toggle-repeat;   keep_open = true; }
      { key = "x";            desc = "Toggle random";     cmd = cfg.exec.toggle-random;   keep_open = true; }
      {
        key = "space";
        desc = "Queue";
        submenu = [
          { key = "a";      desc = "Shuffle library";   cmd = cfg.exec.play-shuffled; }
          { key = "space";  desc = "Play...";           cmd = cfg.exec.search-play; }
          { key = "n";      desc = "Play next...";      cmd = cfg.exec.search-next; }
          { key = "e";      desc = "Enqueue...";        cmd = cfg.exec.search-enqueue; }
        ];
      }
    ];
  };
}
