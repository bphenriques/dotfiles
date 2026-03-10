{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.opencloud;
  collaboraCfg = cfg.services.collabora;
  wopiCfg = cfg.services.wopi;
  oidcCfg = cfg.oidc;

  envFile = "/run/${serviceCfg.name}/env";

  # CSP config to allow external OIDC provider and Collabora
  oidcHost = builtins.replaceStrings ["https://"] [""] oidcCfg.provider.url;
  cspConfig = pkgs.writeText "opencloud-csp.yaml" ''
    directives:
      connect-src:
        - "'self'"
        - "blob:"
        - "${oidcCfg.provider.url}"
        - "wss://${oidcHost}"
        - "${collaboraCfg.publicUrl}"
        - "wss://${collaboraCfg.publicHost}"
      frame-src:
        - "'self'"
        - "${collaboraCfg.publicUrl}"
      script-src:
        - "'self'"
        - "'unsafe-inline'"
        - "'unsafe-eval'"
  '';
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
        systemd.dependentServices = [ "${serviceCfg.name}-env" ];
      };
      oidc = {
        enable = true;
        pkce = true;  # Public client requires PKCE
        callbackURLs = [
          "${serviceCfg.publicUrl}/"
          "${serviceCfg.publicUrl}/oidc-callback.html"
          "${serviceCfg.publicUrl}/oidc-silent-redirect.html"
          "${serviceCfg.publicUrl}/web-oidc-callback"
        ];
        systemd.dependentServices = [ "${serviceCfg.name}-env" ];
      };
      integrations.homepage = {
        enable = true;
        category = "Productivity";
        description = "Cloud Storage & Office";
      };
    };

    # Both don't require additional security. uses WOPI tokens (collabora) or JWE (Wopi)
    collabora.port = 9980;
    wopi.port = 9300;
  };

  # TODO: Consider syncing /var/lib/opencloud to NAS for backup

  services.opencloud = {
    enable = true;
    url = serviceCfg.publicUrl;
    address = serviceCfg.host;
    port = serviceCfg.port;
    environmentFile = envFile;

    environment = {
      # External OIDC (Pocket-ID) - disable built-in IDP
      OC_OIDC_ISSUER = oidcCfg.provider.url; # Does not work when using the yaml based configuration
      OC_EXCLUDE_RUN_SERVICES = "idp";
      OC_ADD_RUN_SERVICES = "collaboration";
    };

    settings = {
      proxy = {
        http.tls = false;                       # Managed by traefik
        autoprovision_accounts = true;          # Users managed externally
        user_cs3_claim = "username";
        csp_config_file_location = toString cspConfig;
      };
      # Point web client directly to OIDC provider
      web.config.oidc = {
        authority = oidcCfg.provider.url;
        metadata_url = "${oidcCfg.provider.url}/.well-known/openid-configuration";
        post_logout_redirect_uri = serviceCfg.publicUrl;
        # client_id: set via WEB_OIDC_CLIENT_ID env var from environmentFile
      };
      collaboration = {
        wopi.wopisrc = wopiCfg.publicUrl; # External WOPI URL that Collabora uses to call back into OpenCloud. Must be routed through Traefik so Collabora can reach it
        app = {
          addr = collaboraCfg.url;
          external_addr = collaboraCfg.publicUrl;
          product = "Collabora";
          proofkeys.disable = true;  # Disable WOPI proof verification
        };
      };
      app-registry = {
        mimetypes = [
          {
            mime_type = "application/vnd.oasis.opendocument.text";
            extension = "odt";
            name = "OpenDocument";
            description = "OpenDocument text document";
            default_app = "Collabora";
            allow_creation = true;
          }
          {
            mime_type = "application/vnd.oasis.opendocument.spreadsheet";
            extension = "ods";
            name = "OpenSpreadsheet";
            description = "OpenDocument spreadsheet";
            default_app = "Collabora";
            allow_creation = true;
          }
          {
            mime_type = "application/vnd.oasis.opendocument.presentation";
            extension = "odp";
            name = "OpenPresentation";
            description = "OpenDocument presentation";
            default_app = "Collabora";
            allow_creation = true;
          }
        ];
      };
    };
  };

  systemd.services."${serviceCfg.name}-env" = {
    description = "Prepare ${serviceCfg.name} environment file";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      User = serviceCfg.name;
      Group = serviceCfg.name;
      RuntimeDirectory = serviceCfg.name;
      RuntimeDirectoryMode = "0750";
      LoadCredential = serviceCfg.oidc.systemd.loadCredentials;
    };
    script = ''
      set -euo pipefail
      cp ${serviceCfg.secrets.templates.env.path} ${envFile}
      # Web client is public (no secret) - only needs client ID
      echo "WEB_OIDC_CLIENT_ID=$(cat "$CREDENTIALS_DIRECTORY/oidc-id")" >> ${envFile}
      chmod 600 ${envFile}
    '';
  };

  # Wire opencloud services to depend on env file
  systemd.services."${serviceCfg.name}-init-config" = {
    requires = [ "${serviceCfg.name}-env.service" ];
    after = [ "${serviceCfg.name}-env.service" ];
  };

  systemd.services.${serviceCfg.name} = {
    requires = [ "${serviceCfg.name}-env.service" ];
    after = [ "${serviceCfg.name}-env.service" ];
  };

  services.collabora-online = {
    enable = true;
    port = collaboraCfg.port;
    settings = {
      protocol = "http";
      ssl = {
        enable = false;
        termination = true;  # TLS terminated at Traefik
      };

      # Protect encoded URLs from being decoded by intermediate proxies (Traefik)
      # Required because OpenCloud embeds the full WOPI URL in the /cool/ path
      hexify_embedded_urls = true;

      net = {
        listen = "any";
        proto = "ipv4";
        post_allow.host = [ "::1" "127.0.0.1" ];
        content_security_policy = "frame-ancestors https://${serviceCfg.publicHost};"; # Allow embedded in OpenCloud iframe
      };
      storage.wopi = {
        "@allow" = true;
        host = [ wopiCfg.publicHost ]; # Must match the hostname in wopisrc (external WOPI URL)
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
