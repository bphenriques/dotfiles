# Pocket-ID: bundled, opinionated OIDC provider — the homelab's first-class implementation of
# the OIDC contract (security/oidc.nix). Provisions users/groups/clients via the pocket-id-manage CLI.
# Expects a host-declared `smtp-password` sops secret (the smtp contract). Fork to vary.
{ config, pkgs, lib, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = config.custom.homelab.services.pocket-id;
  oidcCfg = config.custom.homelab.oidc;
  smtpCfg = config.custom.homelab.smtp;
  port = 8094;
  baseServiceName = "pocket-id-provision-base";

  usersConfigFile = pkgs.writeText "oidc-provision-users-config.json" (builtins.toJSON {
    inherit (oidcCfg.provisionConfig) users groups;
  });

  mkClientConfigFile = name: client:
    pkgs.writeText "oidc-client-config-${name}.json" (builtins.toJSON {
      inherit (client) name callbackURLs pkce allowedGroups;
    });

  pocketIdManage = pkgs.writeShellApplication {
    name = "pocket-id-manage";
    runtimeInputs = [ self.packages.pocket-id-manage ];
    text = ''
      export POCKET_ID_URL="${serviceCfg.url}"
      export POCKET_ID_API_KEY_FILE="${oidcCfg.provider.apiKeyFile}"
      export POCKET_ID_GUESTS_GROUP="${cfg.groups.guests}"
      exec pocket-id-manage-bin "$@"
    '';
  };

  hardenedServiceConfig = {
    Type = "oneshot";
    RemainAfterExit = true;
    Restart = "on-failure";
    RestartSec = 10;
    UMask = "0077";
    PrivateTmp = true;
    ProtectSystem = "strict";
    ReadWritePaths = [ oidcCfg.credentials.dir ];
    ProtectHome = true;
    NoNewPrivileges = true;
    ProtectKernelTunables = true;
    ProtectControlGroups = true;
    RestrictSUIDSGID = true;
    RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
  };
in
{
  options.custom.homelab.oidc.pocket-id.enable =
    lib.mkEnableOption "Pocket-ID OIDC implementation";

  config = lib.mkIf cfg.oidc.pocket-id.enable {
    custom.homelab = {
      services.pocket-id = {
        displayName = "Pocket ID";
        description = "OIDC Provider";
        subdomain = "auth";
        inherit port;
        healthcheck.path = "/health";
        integrations.homepage.enable = true;
        integrations.homepage.tab = "Home";
      };

      oidc.provider = {
        displayName = "Pocket-ID";
        internalName = "PocketID";
        issuerUrl = serviceCfg.publicUrl;
        apiKeyFile = cfg.runtimeSecrets.pocket-id-api-key.path;
      };

      oidc.systemd = {
        baseProvisionUnit = "${baseServiceName}.service";
        clientProvisionUnitPrefix = "pocket-id-provision-client-";
      };

      runtimeSecrets = {
        pocket-id-encryption-key = {
          owner = config.services.pocket-id.user;
          restartUnits = [ "pocket-id.service" ];
        };
        pocket-id-api-key = {
          owner = config.services.pocket-id.user;
          restartUnits = [ "pocket-id.service" ];
        };
      };
    };

    services.pocket-id = {
      enable = true;
      settings = {
        APP_URL = serviceCfg.publicUrl;
        PORT = port;
        HOST = "127.0.0.1";
        TRUST_PROXY = true;
        ANALYTICS_DISABLED = true;
        ENCRYPTION_KEY_FILE = cfg.runtimeSecrets.pocket-id-encryption-key.path;
        ACCENT_COLOR = "default";
        STATIC_API_KEY_FILE = cfg.runtimeSecrets.pocket-id-api-key.path;
        UI_CONFIG_DISABLED = true;

        SMTP_HOST = smtpCfg.host;
        SMTP_PORT = toString smtpCfg.port;
        SMTP_FROM = smtpCfg.from;
        SMTP_USER = smtpCfg.user;
        SMTP_TLS = smtpCfg.tls;
        SMTP_PASSWORD_FILE = config.sops.templates.pocket-id-smtp-password.path;

        # Invite only
        ALLOW_USER_SIGNUPS = "withToken";
        EMAILS_VERIFIED = true;
        EMAIL_VERIFICATION_ENABLED = false;
        EMAIL_ONE_TIME_ACCESS_AS_ADMIN_ENABLED = true;
        EMAIL_ONE_TIME_ACCESS_AS_UNAUTHENTICATED_ENABLED = false;
      };
    };

    sops.templates."pocket-id-smtp-password" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder."smtp-password";
    };

    # Credentials base dir (tmpfs); 0755 so services can traverse to their own 0750 client subdirs.
    systemd.tmpfiles.rules = [
      "d ${oidcCfg.credentials.dir} 0755 root root -"
    ];

    systemd.services = {
      ${baseServiceName} = {
        description = "Pocket-ID users and groups provisioning";
        wantedBy = [ "pocket-id.service" ];
        after = [ "network-online.target" "pocket-id.service" ];
        wants = [ "network-online.target" ];
        requires = [ "pocket-id.service" ];
        partOf = [ "pocket-id.service" ];
        restartTriggers = [ usersConfigFile ];
        startLimitIntervalSec = 300;
        startLimitBurst = 3;
        environment = {
          OIDC_CONFIG_FILE = toString usersConfigFile;
          OIDC_CREDENTIALS_DIR = oidcCfg.credentials.dir;
        };
        serviceConfig = hardenedServiceConfig;
        path = [ pocketIdManage ];
        script = "pocket-id-manage provision-users";
      };
    } // lib.mapAttrs' (name: client: let
      clientConfigFile = mkClientConfigFile name client;
    in lib.nameValuePair "pocket-id-provision-client-${name}" {
      description = "Pocket-ID OIDC client provisioning for ${name}";
      wantedBy = [ "pocket-id.service" ];
      after = [ "network-online.target" "pocket-id.service" "${baseServiceName}.service" ];
      wants = [ "network-online.target" ];
      requires = [ "pocket-id.service" "${baseServiceName}.service" ];
      partOf = [ "pocket-id.service" ];
      restartTriggers = [ clientConfigFile ];
      startLimitIntervalSec = 300;
      startLimitBurst = 3;
      environment = {
        OIDC_CLIENT_CONFIG_FILE = toString clientConfigFile;
        OIDC_CREDENTIALS_DIR = oidcCfg.credentials.dir;
      };
      serviceConfig = hardenedServiceConfig;
      path = [ pocketIdManage ];
      script = "pocket-id-manage provision-client";
    }) oidcCfg.clients;

    environment.systemPackages = [ pocketIdManage ];
  };
}
