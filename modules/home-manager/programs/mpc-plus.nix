{ lib, pkgs, config, self, ... }:
let
  inherit (lib.attrsets) mapAttrs' nameValuePair;
  cfg = config.custom.programs.mpc-plus;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  mpc-plus = lib.getExe cfg.package;

  deviceOpt = lib.types.submodule ({ name, config, ... }:
    {
      options = {
        host  = lib.mkOption { type = lib.types.str; default = name; };
        port  = lib.mkOption { type = lib.types.port; default = 6600; };
      };
    });

  exec = {
    play-pause        = ''${mpc-plus} play-pause'';
    stop              = ''${mpc-plus} stop'';
    play-shuffled     = ''${mpc-plus} play-shuffled'';
    previous          = ''${mpc-plus} previous'';
    next              = ''${mpc-plus} next'';
    clear             = ''${mpc-plus} clear'';
    toggle-random     = ''${mpc-plus} toggle-random'';
    toggle-repeat     = ''${mpc-plus} toggle-repeat'';
    volume-increase   = ''${mpc-plus} volume-increase'';
    volume-decrease   = ''${mpc-plus} volume-decrease'';
    search-play       = ''${mpc-plus} dmenu-file-exec play'';
    search-enqueue    = ''${mpc-plus} dmenu-file-exec add'';
    search-next       = ''${mpc-plus} dmenu-file-exec next'';
    select-server     = ''${mpc-plus} dmenu-select-server'';
  };
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
        deviceIcon = mkIcon "mpc-plus-device" "󰓃";
        errorIcon = mkIcon "mpc-plus-error" "";
      };
    };

    devices = lib.mkOption {
      type = lib.types.attrsOf deviceOpt;
      default = {
        default = {
          host = config.services.mpd.network.listenAddress;
          port = config.services.mpd.network.port;
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.mpc-plus" pkgs lib.platforms.linux) ];

    xdg.configFile= mapAttrs' (name: value: (nameValuePair "mpc-plus/${name}.json" { text = (builtins.toJSON value); })) cfg.devices;

    home.packages = [
      pkgs.mpc
      cfg.package
      (pkgs.makeDesktopItem {
        name = "Music";
        desktopName = "Music";
        icon = mkIcon "music-player" "󰥠";
        exec = ''${lib.getExe config.custom.programs.wlr-which-key.package} mpc-plus'';
        actions = {
          "shuffle"   = { name = "Shuffle library"; icon = (mkIcon "mpc-plus-shuffle-library" ""); exec = exec.play-shuffled; };
          "find-play" = { name = "Play...";         icon = (mkIcon "mpc-plus-find-play" "");       exec = exec.search-play; };
          "stop"      = { name = "Stop";            icon = (mkIcon "mpc-plus-stop" "");            exec = exec.stop; };
        };
      })
    ];

    custom.programs.wlr-which-key.menus.mpc-plus = [
      { key = "p";            desc = "Play/Pause";        cmd = exec.play-pause;      keep_open = true; }
      { key = "s";            desc = "Stop";              cmd = exec.stop; }
      { key = ["Left" "h"];   desc = "Previous";          cmd = exec.previous;        keep_open = true; }
      { key = ["Right" "l"];  desc = "Next";              cmd = exec.next;            keep_open = true; }
      { key = ["Up" "k"];     desc = "Increase volume";   cmd = exec.volume-increase; keep_open = true; }
      { key = ["Down" "j"];   desc = "Reduce volume";     cmd = exec.volume-decrease; keep_open = true; }
      { key = "z";            desc = "Toggle repeat";     cmd = exec.toggle-repeat;   keep_open = true; }
      { key = "x";            desc = "Toggle random";     cmd = exec.toggle-random;   keep_open = true; }
      { key = "d";            desc = "Select Server";     cmd = exec.select-server; }
      {
        key = "space";
        desc = "Queue";
        submenu = [
          { key = "a";      desc = "Shuffle library";   cmd = exec.play-shuffled; }
          { key = "space";  desc = "Play...";           cmd = exec.search-play; }
          { key = "n";      desc = "Play next...";      cmd = exec.search-next; }
          { key = "e";      desc = "Enqueue...";        cmd = exec.search-enqueue; }
        ];
      }
    ];
  };
}
