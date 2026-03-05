{ config, pkgs, lib, ... }:
let
  serviceCfg = config.custom.homelab.services.couchdb;
  adminConfigFile = "${serviceCfg.secrets.secretsDir}/admin.ini";
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.couchdb = {
    port = 5984;
    secrets = {
      files.admin-password = { rotatable = false; };
      systemd.dependentServices = [ "couchdb" "couchdb-configure" ];
    };
  };

  services.couchdb = {
    enable = true;
    port = serviceCfg.port;
    extraConfigFiles = [ adminConfigFile ];
    extraConfig = {
      couchdb.single_node = true;
      chttpd_auth_lockout.mode = "warn";
      chttpd = {
        require_valid_user = true;
        require_valid_user_except_for_up = true;
      };
      chttpd_auth.require_valid_user = true;
      httpd."WWW-Authenticate" = "Basic realm=\"couchdb\"";
    };
  };

  # CouchDB needs RuntimeDirectory for couchdb.uri file
  systemd.services.couchdb.serviceConfig.RuntimeDirectory = "couchdb";

  # Generate admin.ini in secrets service (runs before couchdb)
  systemd.services.homelab-secrets-couchdb.serviceConfig.ExecStartPost = [ (pkgs.writeShellScript "generate-couchdb-admin-ini" ''
    cat > "${adminConfigFile}" <<EOF
[admins]
${config.services.couchdb.adminUser} = $(cat "${serviceCfg.secrets.files.admin-password.path}")
EOF
    chown root:${serviceCfg.secrets.group} "${adminConfigFile}"
    chmod 640 "${adminConfigFile}"
  '') ];
}
