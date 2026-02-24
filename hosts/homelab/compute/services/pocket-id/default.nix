{ config, pkgs, lib, self, ... }:
let
  publicUrl = "https://auth.${self.settings.hosts.compute.domain}";
  encryptionKeyFile = "/var/lib/pocket-id/encryption.key";
  serviceCfg = config.custom.home-server.services.pocket-id;
  port = 8094;
in
{
  imports = [ ./post-start.nix ];

  custom.home-server.services.pocket-id = {
    subdomain = "auth";
    port = port;
  };

  services.pocket-id = {
    enable = true;
    settings = {
      APP_URL = serviceCfg.publicUrl;
      PORT = port;
      HOST = "127.0.0.1";
      TRUST_PROXY = true;
      ANALYTICS_DISABLED = true;
      ENCRYPTION_KEY_FILE = encryptionKeyFile;
      ACCENT_COLOR = "default";
      STATIC_API_KEY_FILE = config.sops.templates.pocket-id-api-key.path;
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
    secrets."pocket-id/api-key".mode = "0400";
    templates."pocket-id-api-key" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder."pocket-id/api-key";
    };

    secrets."pocket-id/smtp-password" = { };
    templates."pocket-id-smtp-password" = {
      owner = config.services.pocket-id.user;
      content = config.sops.placeholder."pocket-id/smtp-password";
    };
  };

  systemd.services.pocket-id.preStart = ''
    if [ ! -f "${encryptionKeyFile}" ]; then
      mkdir -p "$(dirname "${encryptionKeyFile}")"
      ${lib.getExe pkgs.openssl} rand -base64 32 > "${encryptionKeyFile}"
      chown ${config.services.pocket-id.user}:${config.services.pocket-id.group} "${encryptionKeyFile}"
    fi
  '';

  # OIDC provider metadata for consumers
  custom.home-server.oidc.provider = {
    displayName = "Pocket-ID";
    internalName = "PocketID";
    url = publicUrl;
    internalUrl = publicUrl;
    discoveryEndpoint = "${publicUrl}/.well-known/openid-configuration";
    apiKeyFile = config.sops.secrets."pocket-id/api-key".path;
  };
}
