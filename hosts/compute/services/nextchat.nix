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

  custom.ai.nextchat = {
    enable = true;
    port = serviceCfg.port;
    hermesUrl = "http://${hermesApiCfg.host}:${toString hermesApiCfg.port}";
    hermesApiKeyEnvFile = config.sops.templates."nextchat-env".path;
    # Two-backend dropdown — both local. Routing strings are the literal `model`
    # values Hermes resolves:
    #   - `gemma4:e4b`              → default provider (laptop's Ollama)
    #   - `compute_q/gemma4:e2b`    → `custom_providers` entry on the VM
    #                                 (`provider/model` shorthand, verified
    #                                  working via `--provider compute_q
    #                                  --model gemma4:e2b` from the CLI)
    # Cloud is deliberately omitted (privacy boundary, see voice-memo-pipeline.md
    # "Why not cloud"). Default = laptop because compute_q's per-turn prefill
    # latency (~5–10 minutes on the throttled N150) makes it unsuitable as the
    # default interactive backend. Compute stays in the dropdown for the
    # "I know it's slow, I want something" case when laptop is off.
    customModels = [
      config.custom.fleet.ai.model            # laptop primary
      "compute_q/gemma4:e2b"                  # CPU floor on compute
    ];
    defaultModel = config.custom.fleet.ai.model;
  };
}
