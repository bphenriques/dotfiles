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
  exec = {
    menu          = lib.getExe cfg.dmenu;
    screen        = ''${screenshot} screen "${cfg.directory}"'';
    screen-edit   = ''${screenshot} screen-edit'';
    screen-copy   = ''${screenshot} screen-copy'';
    region        = ''${screenshot} region "${cfg.directory}"'';
    region-edit   = ''${screenshot} region-edit'';
    region-copy   = ''${screenshot} region-copy'';
  };

  screenshotActions = [
    { id = "screenshot-screen";       symbol = "󰹑"; label = "Screen (save)";  exec = exec.screen; }
    { id = "screenshot-screen-edit";  symbol = "󰹑"; label = "Screen (edit)";  exec = exec.screen-edit; }
    { id = "screenshot-region";       symbol = ""; label = "Region (save)";  exec = exec.region; }
    { id = "screenshot-region-edit";  symbol = ""; label = "Region (edit)";  exec = exec.region-edit; }
  ];
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

    dmenu = lib.mkOption {
      type = lib.types.package;
      default = self.lib.builders.writeFuzzelDmenuApplication {
        name = "screenshot-menu";
        entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) screenshotActions;
        extraArgs = ''--minimal-lines --hide-prompt'';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.screenshot" pkgs lib.platforms.linux) ];

    home.packages = [
      pkgs.grim
      pkgs.slurp
      pkgs.swappy
      cfg.package
      cfg.dmenu
      (pkgs.makeDesktopItem {
        name = "screenshot-menu";
        desktopName = "Screenshot";
        icon = mkIcon "screenshot" "󰹑";
        exec = lib.getExe cfg.dmenu;
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
          { key = "m"; desc = "Save";  cmd = exec.screen; }
          { key = "c"; desc = "Copy";  cmd = exec.screen-copy; }
          { key = "e"; desc = "Edit";  cmd = exec.screen-edit; }
        ];
      }
      {
        key = "r";
        desc = "Region";
        submenu = [
          { key = "m"; desc = "Save";  cmd = exec.region; }
          { key = "c"; desc = "Copy";  cmd = exec.region-copy; }
          { key = "e"; desc = "Edit";  cmd = exec.region-edit; }
        ];
      }
    ];
  };
}