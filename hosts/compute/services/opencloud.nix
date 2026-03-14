{ config, lib, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.opencloud;
  collaboraCfg = cfg.services.collabora;
  wopiCfg = cfg.services.wopi;
  oidcCfg = cfg.oidc;

  # CSP config to allow external OIDC provider and Collabora
  # https://doc.owncloud.com/ocis/next/deployment/services/s-list/proxy.html#content-security-policy
  # Note: 'unsafe-eval'/'unsafe-inline' are intentionally enabled because OpenCloud Web UI breaks without them.
  oidcHost = builtins.replaceStrings ["https://"] [""] oidcCfg.provider.url;
  yamlFormat = pkgs.formats.yaml { };
  cspConfig = yamlFormat.generate "opencloud-csp.yaml" {
    directives = {
      default-src  = [ "'self'" ];
      script-src   = [ "'self'" "'unsafe-eval'" ];
      style-src    = [ "'self'" "'unsafe-inline'" ];
      font-src     = [ "'self'" ];
      img-src      = [ "'self'" "blob:" "data:" ];
      connect-src  = [ "'self'" "blob:" oidcCfg.provider.url "wss://${oidcHost}" collaboraCfg.publicUrl "wss://${collaboraCfg.publicHost}" ];
      frame-src    = [ "'self'" "blob:" collaboraCfg.publicUrl ];
      frame-ancestors = [ "'self'" ];
    };
  };
in
{
  custom.homelab.services = {
    opencloud = {
      description = "Cloud Storage & Office";
      version = config.services.opencloud.package.version;
      homepage = config.services.opencloud.package.meta.homepage;
      category = "General";
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
            WEB_OIDC_CLIENT_ID=${serviceCfg.oidc.id.placeholder}
          '';
        };
        systemd.dependentServices = [ "opencloud-init-config" "opencloud" ];
      };
      oidc = {
        enable = true;
        pkce = true;
        callbackURLs = [
          "${serviceCfg.publicUrl}/"
          "${serviceCfg.publicUrl}/oidc-callback.html"
          "${serviceCfg.publicUrl}/oidc-silent-redirect.html"
          "${serviceCfg.publicUrl}/web-oidc-callback.html"
        ];
      };
      integrations.homepage.enable = true;
      integrations.homepage.icon = "open-cloud.svg";
      integrations.catalogue.displayName = "OpenCloud";
    };

    # Must be publicly reachable (routed by Traefik) for OpenCloud collaboration flow.
    # OpenCloud sends clients to Collabora, and Collabora calls back into WOPI using public URLs.
    collabora = {
      port = 9980;
      description = "Online Office";
      category = "Infrastructure";
      version = config.services.collabora-online.package.version;
      homepage = config.services.collabora-online.package.meta.homepage;
      integrations.catalogue.enable = false;
    };
    wopi = {
      port = 9300;
      description = "Office Bridge";
      category = "Infrastructure";
      version = config.services.opencloud.package.version;
      homepage = config.services.opencloud.package.meta.homepage;
      integrations.catalogue.enable = false;
    };
  };

  services.opencloud = {
    enable = true;
    url = serviceCfg.publicUrl;
    address = serviceCfg.host;
    port = serviceCfg.port;
    environmentFile = serviceCfg.secrets.templates.env.path;

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
          # Intentionally disabled: proof-key verification currently breaks this setup.
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
