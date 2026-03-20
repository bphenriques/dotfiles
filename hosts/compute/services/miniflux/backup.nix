{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.miniflux;
in
{
  custom.homelab.services.miniflux.backup.package = pkgs.writeShellApplication {
    name = "backup-miniflux";
    runtimeInputs = [ pkgs.curl ];
    text = ''
      export MINIFLUX_URL="${serviceCfg.url}"
      export MINIFLUX_ADMIN_PASSWORD_FILE="${serviceCfg.secrets.files.admin-password.path}"

      # shellcheck disable=SC1091
      source ${./backup.sh}
    '';
  };

  custom.homelab.services.miniflux.secrets.systemd.dependentServices = [ "homelab-backup-miniflux" ];
}
