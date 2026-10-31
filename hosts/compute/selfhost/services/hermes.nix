{ config, ... }:
let
  inherit (config.fleet.ai) assistantEndpoint;
in
{
  # The guest keeps a copy in its own sops: to rotate, delete this file and re-copy it there.
  selfhost.runtimeSecrets."hermes-api-server-key" = { }; # openssl rand -hex 32

  # Fronts the guest's API for phone clients.
  selfhost.services.hermes = {
    displayName = "Hermes";
    meta.description = "Assistant API";
    subdomain = "hermes";
    inherit (assistantEndpoint) host port;
    access.model = "native";        # API_SERVER_KEY is the whole lock
    healthcheck.path = "/health";   # the one route that answers without the key
    systemdServices = [ ];          # the unit runs in the guest, so there is nothing here to attach to
    integrations.homepage.enable = false;   # an API with no browser UI, so a tile would lead nowhere
  };
}
