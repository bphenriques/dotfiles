{ config, ... }:
let
  cfg = config.selfhost;
  alfredVmIp = config.fleet.microvms.compute.agent-alfred-vm;
  alfredVm = import ../../../guests/agent-alfred-vm/settings.nix;
in
{
  # Fronts the guest's API for phone clients. No change to the seal: the guest already admits the
  # gateway on this port, and Traefik runs on the gateway.
  selfhost.services.hermes = {
    displayName = "Hermes";
    meta.description = "Assistant API";
    subdomain = "hermes";
    host = alfredVmIp;
    port = alfredVm.apiPort;
    access.model = "native";        # API_SERVER_KEY is the whole lock
    healthcheck.path = "/health";   # the one route that answers without the key
    systemdServices = [ ];          # the unit runs in the guest, so there is nothing here to attach to
    integrations.homepage.enable = false;   # an API with no browser UI, so a tile would lead nowhere
  };

  selfhost.services.hermes-webui = {
    displayName = "Hermes WebUI";
    meta.description = "Assistant dashboard";
    host = alfredVmIp;
    port = alfredVm.webuiPort;
    access.allowedGroups = [ cfg.groups.admin ];
    access.model = "oidc";
    access.oidc.callbackURLs = [ "${cfg.services.hermes-webui.publicUrl}/api/auth/oidc/callback" ];
    healthcheck.path = "/health";
    systemdServices = [ ];
    extraConfig.landingPage.enable = true;

    # Belt and braces behind Pocket ID: this is the one route whose backend holds the vault.
    traefik.middlewares.hermes-webui-ratelimit.rateLimit = {
      average = 5;
      burst = 10;
    };
  };
}
