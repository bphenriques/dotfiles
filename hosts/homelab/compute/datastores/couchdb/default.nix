{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.couchdb;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.couchdb.port = 5984;

  sops = {
    secrets."couchdb/admin/password" = { };
    templates = {
      "couchdb-admin-password" = {
        owner = config.services.couchdb.user;
        content = config.sops.placeholder."couchdb/admin/password";
      };
      "couchdb-admin" = {
        owner = config.services.couchdb.user;
        content = ''
          [admins]
          ${config.services.couchdb.adminUser} = ${config.sops.placeholder."couchdb/admin/password"}
        '';
      };
    };
  };

  services.couchdb = {
    enable = true;
    port = serviceCfg.port;
    extraConfigFiles = [ config.sops.templates."couchdb-admin".path ];
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
}
