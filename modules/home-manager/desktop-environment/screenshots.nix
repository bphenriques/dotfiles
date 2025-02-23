{ lib, pkgs, config, self, osConfig, ... }:

let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map;
  inherit (lib.attrsets) nameValuePair;

  cfg = config.custom.desktop-environment.screenshots;

  screenshots = lib.getExe self.pkgs.screenshots;
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
  options.custom.desktop-environment.screenshots = {
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
      default = config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR;
    };

    format = lib.mkOption {
      description = "Filename format of screenshots. Templates must be compatible with the date command";
      type = lib.types.str;
      default = "screenshot-%Y%m%d-%H%M%S.png";
    };

    screen      = mkAppOpt' ''${screenshots} screen "${destination}"'';
    screen-copy = mkAppOpt' ''${screenshots} screen-copy "${destination}"'';
    screen-edit = mkAppOpt' ''${screenshots} screen-edit "${destination}"'';

    region      = mkAppOpt' ''${screenshots} region "${destination}"'';
    region-copy = mkAppOpt' ''${screenshots} region-copy "${destination}"'';
    region-edit = mkAppOpt' ''${screenshots} region-edit "${destination}"'';

    dmenu = mkAppOpt' (self.lib.builders.writeDmenuScript pkgs {
      name = "screenshot-dmenu";
      entries = [
        { label = "󰹑    Screenshot screen";           exec = cfg.screen; }
        { label = "󰹑    Screenshot screen (edit)";    exec = cfg.screen-edit; }
        { label = "󰹑    Screenshot screen (copy)";    exec = cfg.screen-copy; }
        { label = "    Screenshot region";           exec = cfg.region; }
        { label = "    Screenshot region (edit)";    exec = cfg.region-edit; }
        { label = "    Screenshot region (copy)";    exec = cfg.region-copy; }
      ];
    });
  };

  config = {
    home.packages = [
      (pkgs.makeDesktopItem {
        name = "screenshot-dmenu";
        desktopName = "Open Screenshot menu";
        icon = "folder";  # FIXME
        exec = cfg.dmenu;
      })
    ];

    xdg.configFile."swappy/config".text = lib.generators.toINI { } {
      Default = {
        save_dir = cfg.directory;
        save_filename_format = cfg.format;
        show_panel = true;
        early_exit = true;
        auto_save = true;
      };
    };

    custom.programs.wlr-which-key.menus.screenshot = lib.mkIf config.custom.programs.wlr-which-key.enable {
      s = submenu "[s]creen" {
        s = cmd "[s]ave"  cfg.screen;
        c = cmd "[c]opy"  cfg.screen-copy;
        e = cmd "[e]dit"  cfg.screen-edit;
      };
      r = submenu "[r]egion" {
        s = cmd "[s]ave"  cfg.region;
        c = cmd "[c]opy"  cfg.region-copy;
        e = cmd "[e]dit"  cfg.region-edit;
      };
    };
  };
}
