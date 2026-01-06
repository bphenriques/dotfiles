{ config, ... }:
let
  port = 8082;
in
{
  custom.home-server.services.pocket-id = {
    internalUrl = "http://127.0.0.1:${toString port}";
  };

  services.pocket-id = {
    enable = true;
    port = port;

    environmentFile = config.sops.templates."pocket-id-env".path;
    settings = {
      APP_URL = "${custom.home-server.services.pocket-id.publicUrl}";
      TRUST_PROXY = true;
      ANALYTICS_DISABLED = true;
      ENCRYPTION_KEY_FILE = "/var/lib/pocket-id/encryption.key";
    };
  };

  # https://github.com/xddxdd/nixos-config/blob/171e7d1a576e63445ae8ad01857067c671578a01/nixos/optional-apps/pocket-id.nix#L14
  # https://github.com/mzonski/nixos-config/blob/8c799ebc51626575375c412a6f0dd3831bcd85dc/nixos-modules/pocket-id-nixos.nix#L34
  # https://github.com/kurnevsky/nixfiles/blob/1cd6ac7e16162adbbc6921fa4ed77e0ba9a1f793/modules/server/pocket-id.nix#L10

  systemd.services.pocket-id = {
    preStart = ''
      if [ ! -f /var/lib/pocket-id/encryption.key ]; then
        mkdir -p /var/lib/pocket-id
        openssl rand -base64 32 > /var/lib/pocket-id/encryption.key
        chown ${config.services.pocket-id.user}:${config.services.pocket-id.group} /var/lib/pocket-id/encryption.key
      fi
    '';
  };

  # I can install pocket-id cli to generate a one-time password.

  # Automatic provisioning is blocked by https://github.com/pocket-id/pocket-id/pull/1205
}