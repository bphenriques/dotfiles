{ config, lib, pkgs, ... }:

with lib;

# Explore https://github.com/khaneliman/dotfiles/tree/main
# https://www.nerdfonts.com/cheat-sheet
# https://github.com/FelixKratz/dotfiles/tree/master
# How to reload

let
  cfg = config.services.sketchybar;
in

{
  options = with types; {
    services.sketchybar.enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to enable the sketchybar.";
    };

    services.sketchybar.package = mkOption {
      type = path;
      default = pkgs.sketchybar;
      description = "The sketchybar package to use.";
    };
  };

  # Run manually with PATH="$PATH:/nix/store/pdc48p1qdjznzdh7bpwhgi5zai7p92hc-sketchybar-2.15.1/bin/" /nix/store/pdc48p1qdjznzdh7bpwhgi5zai7p92hc-sketchybar-2.15.1/bin/sketchybar
  config = mkIf cfg.enable {
    system.defaults.NSGlobalDomain._HIHideMenuBar = true;
    environment.systemPath = ["${cfg.package}/bin"];
    environment.systemPackages = [ cfg.package ];
    launchd.user.agents.sketchybar = {
      serviceConfig.ProgramArguments = [ "${cfg.package}/bin/sketchybar" ];
      serviceConfig.KeepAlive = true;
      serviceConfig.RunAtLoad = true;
      serviceConfig.EnvironmentVariables = {
        PATH = "${cfg.package}/bin:${config.environment.systemPath}";
      };
    };
  };
}
