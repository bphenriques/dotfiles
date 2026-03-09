{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.opencloud;
  collaboraCfg = cfg.services.collabora;
  oidcCfg = cfg.oidc;

  envFile = "/run/opencloud/env";
  wopiPort = 9300;  # Internal WOPI endpoint (not routed via Traefik)
in
{
  custom.homelab.services = {
    opencloud = {
      port = 9200;
      subdomain = "cloud";
      secrets = {
        files = {
          jwt-secret = { rotatable = true; bytes = 32; };
          transfer-secret = { rotatable = true; bytes = 32; };
          machine-id = { rotatable = false; bytes = 16; };
        };
        templates.env = {
          content = ''
            OC_JWT_SECRET=${serviceCfg.secrets.placeholder.jwt-secret}
            OC_TRANSFER_SECRET=${serviceCfg.secrets.placeholder.transfer-secret}
            OC_MACHINE_AUTH_API_KEY=${serviceCfg.secrets.placeholder.machine-id}
          '';
        };
        systemd.dependentServices = [ "opencloud-env" ];
      };
      oidc = {
        enable = true;
        callbackURLs = [ "${serviceCfg.publicUrl}/oidc-callback" ];
        systemd.dependentServices = [ "opencloud-env" ];
      };
      integrations.homepage = {
        enable = true;
        category = "Productivity";
        description = "Cloud Storage & Office";
      };
    };

    collabora = {
      port = 9980;
      subdomain = "collabora";
      # No OIDC (uses WOPI tokens), but require authenticated users at ingress
      # forwardAuth.enable = true; TODO: Test this
    };
  };

  # TODO: Consider syncing /var/lib/opencloud to NAS for backup

  services.opencloud = {
    enable = true;
    url = serviceCfg.publicUrl;
    address = "127.0.0.1";
    port = serviceCfg.port;
    environmentFile = envFile;

    environment = {
      OC_ADD_RUN_SERVICES = "collaboration"; # Enable collaboration service for Collabora integration

      # External OIDC (Pocket-ID) - disable built-in IDP
      OC_OIDC_ISSUER = oidcCfg.provider.url;
      OC_EXCLUDE_RUN_SERVICES = "idp";
      PROXY_AUTOPROVISION_ACCOUNTS = "true";
      PROXY_USER_OIDC_CLAIM = "preferred_username";
      PROXY_USER_CS3_CLAIM = "username";

      # Collaboration/WOPI config (internal - server-to-server)
      COLLABORATION_WOPI_SRC = "http://127.0.0.1:${toString wopiPort}";
      COLLABORATION_APP_ADDR = collaboraCfg.publicUrl;
      COLLABORATION_APP_NAME = "Collabora";
      COLLABORATION_APP_PRODUCT = "Collabora";

      OC_INSECURE = "true"; # TLS terminated at Traefik
    };
  };

  # New service: creates /run/opencloud/env before opencloud services start
  systemd.services.opencloud-env = {
    description = "Prepare OpenCloud environment file";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = "opencloud";
      Group = "opencloud";
      RuntimeDirectory = "opencloud";
      RuntimeDirectoryMode = "0750";
      LoadCredential = serviceCfg.oidc.systemd.loadCredentials;
    };
    script = ''
      set -euo pipefail
      cp ${serviceCfg.secrets.templates.env.path} ${envFile}
      echo "WEB_OIDC_CLIENT_ID=$(cat "$CREDENTIALS_DIRECTORY/oidc-id")" >> ${envFile}
      echo "WEB_OIDC_CLIENT_SECRET=$(cat "$CREDENTIALS_DIRECTORY/oidc-secret")" >> ${envFile}
      chmod 600 ${envFile}
    '';
  };

  # Wire opencloud-init-config to depend on opencloud-env
  systemd.services.opencloud-init-config = {
    requires = [ "opencloud-env.service" ];
    after = [ "opencloud-env.service" ];
  };

  # OpenCloud systemd service configuration
  systemd.services.opencloud = {
    requires = [ "opencloud-env.service" ];
    after = [ "opencloud-env.service" ];
  };

  # Collabora Online NixOS service
  services.collabora-online = {
    enable = true;
    port = collaboraCfg.port;
    settings = {
      ssl = {
        enable = false;
        termination = true;  # TLS terminated at Traefik
      };
      net = {
        listen = "loopback";
        post_allow.host = [ "::1" "127.0.0.1" ];
      };
      storage.wopi = {
        "@allow" = true;
        host = [ "127.0.0.1:${toString wopiPort}" ];
      };
      server_name = collaboraCfg.publicHost;
    };
  };

  # Collabora requires OpenCloud (cannot function without it)
  systemd.services.coolwsd = {
    after = [ "opencloud.service" ];
    requires = [ "opencloud.service" ];
  };
}
