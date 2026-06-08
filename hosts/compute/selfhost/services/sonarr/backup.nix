{ config, pkgs, ... }:
let
  serviceCfg = config.selfhost.services.sonarr;
in
{
  selfhost = {
    services.sonarr.backup.package = pkgs.writeShellApplication {
      name = "backup-sonarr";
      runtimeInputs = [ pkgs.curl ];
      text = ''
        export ARR_URL="${serviceCfg.url}"
        export ARR_API_KEY_FILE="${config.selfhost.runtimeSecrets.sonarr-api-key.path}"

        # shellcheck disable=SC1091
        source ${./backup.sh}
      '';
    };
  };
}
