{ config, lib, pkgs, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.chat;
  model = config.custom.fleet.ai.model;
  dropdownModels = [ model ] ++ config.custom.fleet.ai.extraModels;
  agentVmIp = config.custom.fleet.microvms.compute.agent-vm;
  img = pkgs.containerImages.nextchat;
in
{
  selfhost.services.chat = {
    displayName = "Chat";
    meta.homepage = "https://github.com/ChatGPTNextWeb/NextChat";
    meta.description = "Assistant chat UI";
    meta.category = "productivity";
    port = 3210;
    subdomain = "chat";
    access.allowedGroups = [ cfg.groups.admin ];
    access.model = "forwardAuth";
    extraConfig.landingPage.enable = true;
  };

  selfhost.runtimeTemplates."nextchat-env" = {
    content = "OPENAI_API_KEY=${config.selfhost.runtimePlaceholder."hermes-api-server-key"}\n";
    restartUnits = [ "podman-nextchat.service" ];
  };

  systemd.services.podman-nextchat.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };

  virtualisation.oci-containers.containers.nextchat = {
    image = "${img.image}:v${img.version}";
    autoStart = true;
    environment = {
      HOSTNAME = "127.0.0.1";
      PORT = toString serviceCfg.port;
      BASE_URL = "http://${agentVmIp}:8642"; # hermes API on the bridge
      CUSTOM_MODELS = "-all,${lib.concatMapStringsSep "," (m: "+${m}") dropdownModels}"; # default + extras
      DEFAULT_MODEL = model;
      HIDE_USER_API_KEY = "1";
      HIDE_BALANCE_QUERY = "1";
      DISABLE_FAST_LINK = "1";
      DISABLE_GPT4 = "1";
    };
    environmentFiles = [ config.selfhost.runtimeTemplates."nextchat-env".path ];
    extraOptions = [
      "--network=host"
      "--memory=1g"   # Next.js can OOM-restart on 512m
    ];
  };
}
