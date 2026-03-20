{ config, pkgs, ... }:
let
  actualCfg = config.services.actual;
in
{
  custom.homelab.services.actual-budget.backup = {
    package = pkgs.writeShellApplication {
      name = "backup-actual-budget";
      runtimeInputs = [ pkgs.sqlite ];
      text = ''
        export SERVER_FILES_DIR="${actualCfg.settings.serverFiles}"
        export USER_FILES_DIR="${actualCfg.settings.userFiles}"

        # shellcheck disable=SC1091
        source ${./backup.sh}
      '';
    };
    after = [ "actual.service" ];
  };
}
