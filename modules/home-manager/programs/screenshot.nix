{ lib, pkgs, config, self, osConfig, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib) map nameValuePair;

  cfg = config.custom.programs.screenshot;

  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  screenshot = lib.getExe cfg.package;
  screenshotActions = [
    { id = "screenshot-screen";       symbol = "󰹑"; label = "Screen (save)";  exec = cfg.exec.screen; }
    { id = "screenshot-screen-edit";  symbol = "󰹑"; label = "Screen (edit)";  exec = cfg.exec.screen-edit; }
    { id = "screenshot-region";       symbol = ""; label = "Region (save)";  exec = cfg.exec.region; }
    { id = "screenshot-region-edit";  symbol = ""; label = "Region (edit)";  exec = cfg.exec.region-edit; }
  ];

  dmenu = self.lib.builders.writeDmenuApplication {
    name = "screenshot-menu";
    entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) screenshotActions;
  };
in
{
  options.custom.programs.screenshot = {
    enable = lib.mkEnableOption "custom-screenshot";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.screenshot;
    };
    directory = lib.mkOption {
      description = "Location of screenshots";
      type = lib.types.str;
    };

    exec = {
      menu          = mkAppOpt ''${lib.getExe dmenu}'';
      screen        = mkAppOpt ''${screenshot} screen "${cfg.directory}"'';
      screen-edit   = mkAppOpt ''${screenshot} screen-edit "${cfg.directory}"'';
      region        = mkAppOpt ''${screenshot} region "${cfg.directory}"'';
      region-edit   = mkAppOpt ''${screenshot} region-edit "${cfg.directory}"'';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.screenshot" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.grim
      pkgs.slurp
      pkgs.swappy
      cfg.package
      dmenu
      (pkgs.makeDesktopItem {
        name = "screenshot-menu";
        desktopName = "Screenshot";
        icon = mkIcon "screenshot" "󰹑";
        exec = lib.getExe dmenu;
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

    custom.programs.wlr-which-key.menus.screenshot = [
      {
        key = "s";
        desc = "Screen";
        submenu = [
          { key = "m"; desc = "Save";  cmd = ''${screenshot} screen "${cfg.directory}"''; }
          { key = "c"; desc = "Copy";  cmd = ''${screenshot} screen-copy''; }
          { key = "e"; desc = "Edit";  cmd = ''${screenshot} screen-edit''; }
        ];
      }
      {
        key = "r";
        desc = "Region";
        submenu = [
          { key = "m"; desc = "Save";  cmd = ''${screenshot} region "${cfg.directory}"''; }
          { key = "c"; desc = "Copy";  cmd = ''${screenshot} region-copy''; }
          { key = "e"; desc = "Edit";  cmd = ''${screenshot} region-edit''; }
        ];
      }
    ];
  };
}