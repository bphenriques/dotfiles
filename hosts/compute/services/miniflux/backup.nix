{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  backupCfg = config.custom.homelab.backup;
in
{
  custom.homelab.services.miniflux.backup = {
    script = ./backup.sh;
    environment = {
      MINIFLUX_URL = serviceCfg.url;
      MINIFLUX_ADMIN_PASSWORD_FILE = serviceCfg.secrets.files.admin-password.path;
      OUTPUT_DIR = "${backupCfg.extrasDir}/miniflux";
    };
  };

  custom.homelab.services.miniflux.secrets.systemd.dependentServices = [ "homelab-backup-miniflux" ];
}
