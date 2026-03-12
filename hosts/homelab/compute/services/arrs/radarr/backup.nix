{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.radarr;
  backupCfg = config.custom.homelab.backup;
in
{
  custom.homelab.services.radarr.backup = {
    script = ./backup.sh;
    environment = {
      ARR_URL = serviceCfg.url;
      ARR_API_KEY_FILE = serviceCfg.secrets.files.api-key.path;
      OUTPUT_DIR = "${backupCfg.extrasDir}/radarr";
    };
  };

  custom.homelab.services.radarr.secrets.systemd.dependentServices = [ "homelab-backup-radarr" ];
}
