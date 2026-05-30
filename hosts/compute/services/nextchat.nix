{ config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = config.custom.homelab.services.nextchat;
  hermesApiCfg = config.custom.homelab.services.hermes-api;
  img = pkgs.containerImages.nextchat;
in
{
  custom.homelab.services.nextchat = {
    displayName = "Chat";
    metadata.description = "Chat UI for the Hermes assistant";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Productivity";
    port = 3210;
    subdomain = "chat";
    access.allowedGroups = with cfg.groups; [ admin ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
  };

  sops = {
    secrets."hermes-agent/api-server-key" = {
      owner = "root";
      mode = "0440";
    };
    templates."nextchat-env" = {
      content = ''
        OPENAI_API_KEY=${config.sops.placeholder."hermes-agent/api-server-key"}
      '';
      owner = "root";
      mode = "0440";
    };
  };

  custom.ai.chat = {
    enable = true;
    port = serviceCfg.port;
    hermesUrl = "http://${hermesApiCfg.host}:${toString hermesApiCfg.port}";
    hermesApiKeyEnvFile = config.sops.templates."nextchat-env".path;
  };
}
