{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
  backupCfg = config.custom.homelab.backup;
in
{
  custom.homelab.services.miniflux.backup.package = pkgs.writeShellApplication {
    name = "backup-miniflux";
    runtimeInputs = [ pkgs.curl ];
    text = ''
      export MINIFLUX_URL="${serviceCfg.url}"
      export MINIFLUX_ADMIN_PASSWORD_FILE="${serviceCfg.secrets.files.admin-password.path}"
      export OUTPUT_DIR="${backupCfg.extrasDir}/miniflux"

      # shellcheck disable=SC1091
      source ${./backup.sh}
    '';
  };

  custom.homelab.services.miniflux.secrets.systemd.dependentServices = [ "homelab-backup-miniflux" ];
}
