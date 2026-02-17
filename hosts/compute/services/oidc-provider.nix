# OIDC Provider Configuration
#
# Configures the OIDC provider metadata and API access for compute host.
# The actual Pocket-ID service runs in the auth VM.

{ config, self, ... }:
let
  publicUrl = "https://auth.${self.settings.compute.domain}";
in
{
  # OIDC provider metadata (Pocket-ID running in auth VM)
  custom.home-server.oidc.provider = {
    displayName = "Pocket-ID";
    internalName = "PocketID";
    url = publicUrl;
    discoveryEndpoint = "${publicUrl}/.well-known/openid-configuration";
    apiKeyFile = config.sops.secrets."pocket-id/api-key".path;
  };

  sops.secrets."pocket-id/api-key" = {
    mode = "0400";
  };
}
