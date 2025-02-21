{ lib, pkgs, config, self, osConfig, ... }:

let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map;
  inherit (lib.attrsets) nameValuePair;

  cfg = config.custom.desktop-environment.screen-recorder;

  screen-recorder = lib.getExe self.pkgs.screen-recorder;
  date = lib.getExe' pkgs.coreutils "date";
  destination = "$(${date} +'${cfg.directory}/${cfg.format}')";

  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
in
{
  options.custom.desktop-environment.screen-recorder = {
    directory = lib.mkOption {
      description = "Location of recordings";
      type = lib.types.str;
      default = config.xdg.userDirs.extraConfig.XDG_RECORDINGS_DIR;
    };

    format = lib.mkOption {
       description = "Filename format of recordings. Templates must be compatible with the date command";
       type = lib.types.str;
       default = "record-%Y%m%d-%H%M%S.mp4";
    };

    screen-audio     = mkAppOpt' ''${screen-recorder} screen-audio "${destination}"'';
    screen-no-audio  = mkAppOpt' ''${screen-recorder} screen-no-audio "${destination}"'';
    region-audio     = mkAppOpt' ''${screen-recorder} region-audio "${destination}"'';
    region-no-audio  = mkAppOpt' ''${screen-recorder} region-no-audio "${destination}"'';
    stop             = mkAppOpt' ''${screen-recorder} stop'';

    dmenu = mkAppOpt' (self.lib.builders.writeDmenuScript pkgs {
      name = "screen-recorder-dmenu";
      # TODO: Menu start: 󰑋
      entries = [
        { label = "󰹑    Record screen (with audio)";  exec = cfg.screen-audio; }
        { label = "󰹑    Record screen (no audio)";    exec = cfg.screen-no-audio; }
        { label = "    Record region (with audio)";  exec = cfg.region-audio; }
        { label = "    Record region (no audio)";    exec = cfg.region-no-audio; }
        { label = "    Stop recording";              exec = cfg.stop; }
      ];
    });
  };

  config = {
    home.packages = [
      pkgs.wl-screenrec
      self.pkgs.screen-recorder

      (pkgs.makeDesktopItem {
        name = "screen-recoder-dmenu";
        desktopName = "Open screen-recorder menu";
        icon = "folder";  # FIXME
        exec = cfg.dmenu;
      })
    ];

    custom.programs.wlr-which-key.menus.screen-recorder = lib.mkIf config.custom.programs.wlr-which-key.enable {
      S = cmd "[S]top current recording" cfg.stop;
      s = submenu "Record [s]creen" {
        a = cmd "with [a]udio"  cfg.screen-audio;
        n = cmd "[n]o audio"    cfg.screen-no-audio;
      };
      r = submenu "Record [r]egion" {
        a = cmd "with [a]udio"  cfg.region-audio;
        n = cmd "[n]o audio"    cfg.region-no-audio;
      };
    };
  };
}
