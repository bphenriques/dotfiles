{ config, pkgs, lib, self, ... }:
let
  serviceCfg = config.custom.homelab.services.radicale;
  usernames = builtins.attrNames (lib.filterAttrs (_: u: u.services.radicale.enable) config.custom.homelab.users);

  htpasswdFile = "/var/lib/radicale/users";
  configFile = pkgs.writeText "radicale-configure.json" (builtins.toJSON {
    inherit htpasswdFile;
    users = lib.listToAttrs (map (uname: {
      name = uname;
      value = { passwordFile = serviceCfg.secrets.files."password-${uname}".path; };
    }) usernames);
  });
in
{
  systemd.services.radicale-configure = {
    description = "Generate Radicale htpasswd from homelab users";
    requiredBy = [ "radicale.service" ];
    before = [ "radicale.service" ];
    restartTriggers = [ configFile ./configure.nu ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = 600;
      Restart = "on-failure";
      RestartSec = 10;
      UMask = "0027";
    };
    startLimitIntervalSec = 300;
    startLimitBurst = 3;
    environment.RADICALE_PROVISION_FILE = configFile;
    path = [ pkgs.apacheHttpd pkgs.nushell pkgs.coreutils ];
    script = ''nu ${self.lib.builders.writeNushellScript "radicale-configure" ./configure.nu}'';
  };
}
