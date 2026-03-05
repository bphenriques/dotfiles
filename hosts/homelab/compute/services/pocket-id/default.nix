{ config, pkgs, lib, self, ... }:
let
  publicUrl = "https://auth.${self.settings.hosts.compute.domain}";
  serviceCfg = config.custom.homelab.services.pocket-id;
  port = 8094;
in
{
  imports = [ ./configure.nix ];

  custom.homelab = {
    services.pocket-id = {
      subdomain = "auth";
      port = port;
      secrets = {
        files = {
          encryption-key = { rotatable = true; };
          api-key = { rotatable = true; };
        };
        systemd.dependentServices = [ "pocket-id" ];
      };
      integrations.homepage = {
        enable = true;
        category = "Infrastructure";
        description = "OIDC Provider";
      };
    };

    oidc.provider = {
      displayName = "Pocket-ID";
      internalName = "PocketID";
      url = publicUrl;
      internalUrl = publicUrl;
      discoveryEndpoint = "${publicUrl}/.well-known/openid-configuration";
      apiKeyFile = serviceCfg.secrets.files.api-key.path;
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
      ENCRYPTION_KEY_FILE = serviceCfg.secrets.files.encryption-key.path;
      ACCENT_COLOR = "default";
      STATIC_API_KEY_FILE = serviceCfg.secrets.files.api-key.path;
      UI_CONFIG_DISABLED = true;

      # SMTP configuration
      SMTP_HOST = self.settings.smtp.host;
      SMTP_PORT = toString self.settings.smtp.port;
      SMTP_FROM = self.settings.smtp.from;
      SMTP_USER = self.settings.smtp.user;
      SMTP_TLS = self.settings.smtp.tls;
      SMTP_PASSWORD_FILE = config.sops.templates.pocket-id-smtp-password.path;

      # Invite only
      ALLOW_USER_SIGNUPS = "withToken";
      EMAILS_VERIFIED = true;
      EMAIL_VERIFICATION_ENABLED = false;
      EMAIL_ONE_TIME_ACCESS_AS_ADMIN_ENABLED = true;
      EMAIL_ONE_TIME_ACCESS_AS_UNAUTHENTICATED_ENABLED = false;
    };
  };

  sops = {
    secrets."smtp-password" = { };
    templates."pocket-id-smtp-password" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder."smtp-password";
    };
  };
}
