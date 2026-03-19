{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.radarr;
  backupCfg = config.custom.homelab.backup;
in
{
  custom.homelab.services.radarr.backup.package = pkgs.writeShellApplication {
    name = "backup-radarr";
    runtimeInputs = [ pkgs.curl ];
    text = ''
      export ARR_URL="${serviceCfg.url}"
      export ARR_API_KEY_FILE="${serviceCfg.secrets.files.api-key.path}"
      export OUTPUT_DIR="${backupCfg.extrasDir}/radarr"

      # shellcheck disable=SC1091
      source ${./backup.sh}
    '';
  };

  custom.homelab.services.radarr.secrets.systemd.dependentServices = [ "homelab-backup-radarr" ];
}
