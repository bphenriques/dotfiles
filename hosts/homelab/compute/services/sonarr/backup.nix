{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.sonarr;
  backupCfg = config.custom.homelab.backup;
in
{
  custom.homelab.services.sonarr.backup = {
    script = ./backup.sh;
    environment = {
      ARR_URL = serviceCfg.url;
      ARR_API_KEY_FILE = serviceCfg.secrets.files.api-key.path;
      OUTPUT_DIR = "${backupCfg.extrasDir}/sonarr";
    };
  };

  custom.homelab.services.sonarr.secrets.systemd.dependentServices = [ "homelab-backup-sonarr" ];
}
