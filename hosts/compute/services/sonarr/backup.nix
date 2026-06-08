{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.sonarr;
in
{
  custom.homelab = {
    services.sonarr.backup.package = pkgs.writeShellApplication {
      name = "backup-sonarr";
      runtimeInputs = [ pkgs.curl ];
      text = ''
        export ARR_URL="${serviceCfg.url}"
        export ARR_API_KEY_FILE="${config.custom.homelab.runtimeSecrets.sonarr-api-key.path}"

        # shellcheck disable=SC1091
        source ${./backup.sh}
      '';
    };
  };
}
