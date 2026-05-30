{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.gitea;
  oidcCfg = config.custom.homelab.oidc;
in
{
  imports = [ ./configure.nix ];

  options.custom.homelab.users = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.services.gitea = {
        enable = lib.mkEnableOption "Gitea account for this user (provisioned at configure time)";
        isAdmin = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Grant Gitea site-admin to this user. Honored only at user creation;
            once the row exists, gitea-configure does not promote/demote.
          '';
        };
      };
    });
  };

  # Non-human identities (microvm agents, CI bots, …). Distinct from `users`
  # because the concerns differ: no UI, no group permissions, no OIDC — just
  # "this machine needs to authenticate against service X."
  options.custom.homelab.serviceAccounts = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options = {
        description = lib.mkOption {
          type = lib.types.str;
          description = "Human-readable note on what this identity is for.";
        };
        services.gitea = {
          enable = lib.mkEnableOption "Gitea account for this service identity";
          sshKeys = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Public SSH keys (authorized_keys format) registered to this account for git operations.";
          };
        };
      };
    });
    default = { };
  };

  config = {
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
        # Built-in SSH server for git operations — reachable on the LAN
        # (or via wireguard from outside). 2222 to avoid clashing with
        # compute's host sshd; clone URLs become ssh://git@host:2222/...
        START_SSH_SERVER = true;
        SSH_LISTEN_HOST = "0.0.0.0";
        SSH_LISTEN_PORT = 2222;
        SSH_PORT = 2222;
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

  # Gitea's built-in SSH server on the LAN interface — reachable from the
  # microvm bridge (trusted) and over wireguard. Not exposed to the WAN.
  networking.firewall.interfaces.bond0.allowedTCPPorts = [ 2222 ];
  };
}
