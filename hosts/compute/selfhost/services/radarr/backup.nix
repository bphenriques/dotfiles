{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.radarr;
in
{
  selfhost = {
    services.radarr.backup.package = pkgs.writeShellApplication {
      name = "backup-radarr";
      runtimeInputs = [ pkgs.curl ];
      text = ''
        export ARR_URL="${serviceCfg.url}"
        export ARR_API_KEY_FILE="${config.selfhost.runtimeSecrets.radarr-api-key.path}"

        # shellcheck disable=SC1091
        source ${./backup.sh}
      '';
    };
  };
}
