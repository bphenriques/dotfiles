{ config, lib, self, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.grist;
  adminUsers = lib.filterAttrs (_: u: u.isAdmin) config.custom.homelab.users;
  adminEmail = (lib.head (lib.attrValues adminUsers)).email;
in
{
  custom.homelab.services.grist.backup = {
    package = pkgs.writeShellApplication {
      name = "backup-grist";
      runtimeInputs = [ pkgs.nushell ];
      text = ''
        export GRIST_URL="${serviceCfg.url}"
        export GRIST_ADMIN_EMAIL="${adminEmail}"

        nu ${self.lib.builders.writeNushellScript "backup-grist" ./backup.nu}
      '';
    };
    after = [ "podman-grist.service" ];
  };
}
