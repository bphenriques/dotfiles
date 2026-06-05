{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.radarr;
in
{
  custom.homelab = {
    services.radarr.backup.package = pkgs.writeShellApplication {
      name = "backup-radarr";
      runtimeInputs = [ pkgs.curl ];
      text = ''
        export ARR_URL="${serviceCfg.url}"
        export ARR_API_KEY_FILE="${config.custom.homelab.runtimeSecrets.radarr-api-key.path}"

        # shellcheck disable=SC1091
        source ${./backup.sh}
      '';
    };

    runtimeSecrets.radarr-api-key.restartUnits = [ "homelab-backup-backblaze-radarr.service" ];
  };
}
