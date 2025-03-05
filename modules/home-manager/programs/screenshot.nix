{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs replaceStrings;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.screenshot;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  cmd = desc: cmd: { inherit desc cmd; };
  submenu = desc: submenu: { inherit desc submenu; };
  mkIcon = name: symbol: self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } name symbol;

  screenshot = lib.getExe self.pkgs.screenshot;
  screenshotActions = [
    { id = "screenshot-screen";    symbol = "󰹑"; label = "Screen";  exec = cfg.exec.screen; }
    { id = "screenshot-region";    symbol = ""; label = "Region";  exec = cfg.exec.region; }
  ];
in
{
  options.custom.programs.screenshot = {
    enable = lib.mkEnableOption "customn-screenshot";
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
    };

    exec = {
      screen  = mkAppOpt ''${screenshot} screen "${cfg.directory}"'';
      region  = mkAppOpt ''${screenshot} region "${cfg.directory}"'';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.screenshot" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.grim
      pkgs.slurp
      pkgs.swappy
      self.pkgs.screenshot
      (pkgs.makeDesktopItem {
        name = "screenshot-menu";
        desktopName = "Screenshot";
        icon = mkIcon "screenshot" "󰹑";
        exec = lib.getExe (self.lib.builders.writeDmenuApplication pkgs {
          name = "screenshot-menu";
          entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) screenshotActions;
        });
        actions = let
          toAction = b: nameValuePair b.id {
            name = b.label;
            icon = mkIcon b.id b.symbol;
            exec = b.exec;
          };
        in listToAttrs (lib.map toAction screenshotActions);
      })
    ];

    custom.programs.swappy.enable = true;
    custom.programs.swappy.directory = cfg.directory;

    # Limitation on the yaml generation that breaks the file if the line gets long (the full exe + arg)
    custom.programs.wlr-which-key.menus.screenshot = {
      s = submenu "Screen" {
        s = cmd "Save"  ''screenshot screen "${cfg.directory}"'';
        c = cmd "Copy"  ''screenshot screen-copy'';
      };
      r = submenu "Region" {
        s = cmd "Save"  ''screenshot region "${cfg.directory}"'';
        c = cmd "Copy"  ''screenshot region-copy'';
      };
    };
  };
}
