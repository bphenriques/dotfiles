{ config, lib, self, ... }:

let
  cfg = config.custom.programs.project;
in {
  options.custom.programs.project = {
    enable = lib.mkEnableOption "project";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.project;
      description = "package to install.";
    };

    directory = lib.mkOption {
      type = lib.types.str;
      default = config.xdg.userDirs.documents;
      description = "directory where projects live";
    };

    enableFishIntegration = lib.mkOption {
      type = lib.types.bool;
      description = "Whether to enable Fish integration.";
      default = true;
    };

    fishKeybinding = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      description = "Keybinding to access widget";
      default = "alt-p";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    home.sessionVariables.PROJ_ROOT = cfg.directory;
    programs.fish = lib.mkIf cfg.enableFishIntegration {
      plugins = [ { name = "project"; src = "${cfg.package.src}/fish-plugin"; } ];
      functions.fish_user_key_bindings = lib.mkIf (cfg.fishKeybinding != null) ''bind ${cfg.fishKeybinding} __project-widget'';
    };
  };
}
