{ config, lib, pkgs, ... }:

with lib;
with types;

let
  cfg = config.programs.direnv.extra;
in
{
  options.programs.direnv.extra = {
    disableLogging = mkEnableOption "direnv-extra-disable-log";
  };

  config.home.sessionVariables = mkIf cfg.disableLogging {
    DIRENV_LOG_FORMAT    = "";              # Remove dir-env messages.
  };
}
