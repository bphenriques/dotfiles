{ config, lib, self, ... }:

let
  cfg = config.custom.programs.fzf-rg;
in {
  options.custom.programs.fzf-rg = {
    enable = lib.mkEnableOption "fzf-rg";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.fzf-rg;
      description = "package to install.";
    };

    enableFishIntegration = lib.mkOption {
      type = lib.types.bool;
      description = "Whether to enable Fish integration.";
      default = true;
    };

    fishKeybinding = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      description = "Keybinding to access widget";
      default = "alt-.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    programs.fish = lib.mkIf cfg.enableFishIntegration {
      plugins = [ { name = "fzf-rg"; src = "${cfg.package.src}/fish-plugin"; } ];
      functions.fish_user_key_bindings = lib.mkIf (cfg.fishKeybinding != null) ''bind ${cfg.fishKeybinding} __frg-widget'';
    };
  };
}
