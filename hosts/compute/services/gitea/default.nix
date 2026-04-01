{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.gitea;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ];

  custom.homelab.services.gitea = {
    displayName = "Gitea";
    metadata.description = "Git Server";
    metadata.version = config.services.gitea.package.version;
    metadata.homepage = config.services.gitea.package.meta.homepage;
    metadata.category = "Productivity";
    port = 3100;
    subdomain = "git";
    access.allowedGroups = with config.custom.homelab.groups; [ admin ];
    oidc = {
      enable = true;
      callbackURLs = [ "${serviceCfg.publicUrl}/user/oauth2/${oidcCfg.provider.internalName}/callback" ];
      systemd.dependentServices = [ "gitea" "gitea-configure" ];
    };
    healthcheck.path = "/api/healthz";
    integrations.homepage.enable = true;

    backup = {
      package = pkgs.writeShellApplication {
        name = "backup-gitea";
        text = ''cp -a "${config.services.gitea.repositoryRoot}/." "$OUTPUT_DIR/"'';
      };
      after = [ "gitea.service" ];
    };
  };

  services.gitea = {
    enable = true;
    database.type = "sqlite3";
    settings = {
      server = {
        HTTP_ADDR = "127.0.0.1";
        HTTP_PORT = serviceCfg.port;
        ROOT_URL = serviceCfg.publicUrl;
        DOMAIN = serviceCfg.publicHost;
      };
      service = {
        DISABLE_REGISTRATION = true;
        ENABLE_NOTIFY_MAIL = false;
        ENABLE_BASIC_AUTHENTICATION = false;
      };
      session.PROVIDER = "file";
      oauth2.ENABLED = false; # Do not gitea acting as Oauth2
      "oauth2_client" = {
        ENABLE_AUTO_REGISTRATION = true;
        USERNAME = "nickname";
        ACCOUNT_LINKING = "auto";
        UPDATE_AVATAR = true;
      };
      openid = {
        ENABLE_OPENID_SIGNIN = false;
        ENABLE_OPENID_SIGNUP = false;
      };
      repository = {
        DEFAULT_PRIVATE = "private";
        ENABLE_PUSH_CREATE_USER = true;
      };
      ui.DEFAULT_THEME = "gitea-dark";

      # Features I do not need
      mailer.ENABLED = false;
      packages.ENABLED = false;
      "cron.update_checker".ENABLED = false;
      indexer.REPO_INDEXER_ENABLED = false;
      other.SHOW_FOOTER_VERSION = false;
      actions.ENABLED = false;
    };
  };

  systemd.services.gitea.serviceConfig.SupplementaryGroups = serviceCfg.oidc.systemd.supplementaryGroups;
}
