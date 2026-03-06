# CouchDB Datastore
#
# Used by: Obsidian LiveSync
{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.couchdb;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.couchdb = {
    port = 5984;
    secrets = {
      files.admin-password = { rotatable = false; };
      templates."admin.ini".content = ''
        [admins]
        ${config.services.couchdb.adminUser} = ${serviceCfg.secrets.placeholder.admin-password}
      '';
      systemd.dependentServices = [ "couchdb" "couchdb-configure" ];
    };
  };

  services.couchdb = {
    enable = true;
    port = serviceCfg.port;
    extraConfigFiles = [ serviceCfg.secrets.templates."admin.ini".path ];
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

  systemd.services.couchdb.serviceConfig.RuntimeDirectory = "couchdb";
}
