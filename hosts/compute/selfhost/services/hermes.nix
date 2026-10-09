{ config, ... }:
let
  agentVmIp = config.fleet.microvms.compute.agent-vm;
  agentVm = import ../../../guests/agent-vm/settings.nix;
in
{
  # Fronts the guest's API for phone clients. No change to the seal: the guest already admits the
  # gateway on this port, and Traefik runs on the gateway.
  selfhost.services.hermes = {
    displayName = "Hermes";
    meta.description = "Assistant API";
    subdomain = "hermes";
    host = agentVmIp;
    port = agentVm.apiPort;
    access.model = "native";        # API_SERVER_KEY is the whole lock
    healthcheck.path = "/health";   # the one route that answers without the key
    systemdServices = [ ];          # the unit runs in the guest, so there is nothing here to attach to
    integrations.homepage.enable = false;   # an API with no browser UI, so a tile would lead nowhere
  };

  selfhost.services.hermes-webui = {
    displayName = "Hermes WebUI";
    meta.description = "Assistant dashboard";
    host = agentVmIp;
    port = agentVm.webuiPort;
    access.model = "native";        # its own password gate, which is the only one its phone client speaks
    healthcheck.path = "/health";
    systemdServices = [ ];
    extraConfig.landingPage.enable = true;

    # A static password is the whole lock, so bound how fast it can be guessed.
    traefik.middlewares.hermes-webui-ratelimit.rateLimit = {
      average = 5;
      burst = 10;
    };
  };
}
