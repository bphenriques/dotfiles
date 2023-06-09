{ config, lib, pkgs, ... }:

with lib;

# Explore https://github.com/khaneliman/dotfiles/tree/main
# https://www.nerdfonts.com/cheat-sheet
# https://github.com/FelixKratz/dotfiles/tree/master

let
  cfg = config.services.sketchybar;
  debugPath = "/tmp/sketchybar.log";
  debugPlist = {
    StandardOutPath = debugPath;
    StandardErrorPath = debugPath;
  };
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

    services.sketchybar.dependencies = mkOption {
      type = listOf path;
      default = [];
      description = "Additional dependencies in runtime (e.g. jq)";
    };

    services.sketchybar.debug = mkOption {
      type = bool;
      default = false;
      description = "If debug should be enabled. If so ${debugPath} will be used.";
    };
  };

  config = mkIf cfg.enable {
    system.defaults.NSGlobalDomain._HIHideMenuBar = true;
    environment.systemPackages = [ cfg.package ];
    launchd.user.agents.sketchybar = {
      serviceConfig = {
        ProgramArguments = [ "${cfg.package}/bin/sketchybar" ];
        KeepAlive = true;
        RunAtLoad = true;
        ProcessType = "Interactive";
        EnvironmentVariables = {
          PATH =
            let
              basePaths =[ "${cfg.package}/bin" "${config.environment.systemPath}" ];
              extraPath = (map (dep: "${dep}/bin") cfg.dependencies);
            in concatStringsSep ":" (basePaths ++ extraPath);
        };
      } // optionalAttrs cfg.debug debugPlist;
    };
  };
}
