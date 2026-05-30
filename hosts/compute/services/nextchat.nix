{ config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = config.custom.homelab.services.nextchat;
  hermesApiCfg = config.custom.homelab.services.hermes-api;
  img = pkgs.containerImages.nextchat;
in
{
  # Friendly chat surface fronting the Hermes API. The Hermes built-in
  # dashboard at hermes.{domain} keeps the admin/TUI view.
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

  # Decrypt the VM's API token on compute — the same plaintext lives in
  # both compute's and personal-agent's sops files (the VM accepts it as bearer,
  # NextChat sends it). Keep them in sync at rotation time.
  sops.secrets."hermes-agent/api-server-key" = {
    owner = "root";
    mode = "0440";
  };
  sops.templates."nextchat-env" = {
    content = ''
      OPENAI_API_KEY=${config.sops.placeholder."hermes-agent/api-server-key"}
    '';
    owner = "root";
    mode = "0440";
  };

  # Container runtime, env wiring, and resource limits live in the AI
  # module. This file owns the homelab framework registration (Traefik
  # routing, ACLs) + the secret plumbing.
  custom.ai.chat = {
    enable = true;
    port = serviceCfg.port;
    hermesUrl = "http://${hermesApiCfg.host}:${toString hermesApiCfg.port}";
    hermesApiKeyEnvFile = config.sops.templates."nextchat-env".path;
  };
}
