{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.ntfy;
  ntfy = "${lib.getExe config.services.ntfy-sh.package}";
in
{
  custom.homelab.services.ntfy.secrets = {
    files.admin-password = { rotatable = true; };
    systemd.dependentServices = [ "ntfy-sh" "ntfy-configure" ];
  };

  systemd.services.ntfy-configure = {
    description = "ntfy setup";
    wantedBy = [ "ntfy-sh.service" ];
    after = [ "ntfy-sh.service" ];
    requires = [ "ntfy-sh.service" ];
    partOf = [ "ntfy-sh.service" ];
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      Restart = "on-failure";
      RestartSec = 10;
    };
    script = ''
      set -euo pipefail
      export NTFY_PASSWORD="$(cat ${serviceCfg.secrets.files.admin-password.path})"

      # Create admin user if missing, otherwise update password (rotation-safe)
      ${ntfy} user add --role=admin --ignore-exists admin
      ${ntfy} user change-pass admin

      # Household members can subscribe to media without credentials
      ${ntfy} access everyone media ro
    '';
  };
}
