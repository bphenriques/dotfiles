{ config, pkgs, ... }:
{
  imports = [
    ./agents/recipe-to-cooklang
  ];

  # Ollama — local LLM inference with CUDA
  services.ollama = {
    enable = true;
    package = pkgs.ollama-cuda;
    loadModels = [ "qwen3:8b" "qwen2.5vl:3b" "nomic-embed-text" ];
    environmentVariables = {
      # 8 GB VRAM is shared with PRIME-sync desktop (Firefox/IDE). 2 min keeps
      # the model warm during interactive bursts but frees VRAM soon after.
      OLLAMA_KEEP_ALIVE = "2m";
      OLLAMA_MAX_LOADED_MODELS = "1";
      OLLAMA_NUM_PARALLEL = "1";
    };
  };

  # Open WebUI — local chat surface (localhost only).
  services.open-webui = {
    enable = true;
    port = 8080;
    environment = {
      ANONYMIZED_TELEMETRY = "False";
      DO_NOT_TRACK = "True";
      SCARF_NO_ANALYTICS = "True";
      ENABLE_COMMUNITY_SHARING = "False";
      OLLAMA_BASE_URL = "http://127.0.0.1:11434";
      WEBUI_AUTH = "False";
    };
  };
}
