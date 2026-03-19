{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.tandoor;
  backupCfg = config.custom.homelab.backup;
  tandoorCfg = config.services.tandoor-recipes;
  pgPackage = config.services.postgresql.package;
in
{
  custom.homelab.services.tandoor.backup = {
    package = pkgs.writeShellApplication {
      name = "backup-tandoor";
      runtimeInputs = [ pkgs.util-linux pgPackage ];
      text = ''
        export DB_NAME="${tandoorCfg.user}"
        export DB_USER="${tandoorCfg.user}"
        export MEDIA_DIR="/var/lib/tandoor-recipes/media"
        export OUTPUT_DIR="${backupCfg.extrasDir}/tandoor"

        # shellcheck disable=SC1091
        source ${./backup.sh}
      '';
    };
    after = [ "tandoor-recipes.service" "postgresql.service" ];
  };
}
