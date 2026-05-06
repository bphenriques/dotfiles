{ config, pkgs, ... }:
{
  # Ollama — local LLM inference with CUDA
  services.ollama = {
    enable = true;
    package = pkgs.ollama-cuda;
    loadModels = [ "qwen3:8b" "qwen2.5:7b" "qwen2.5vl:3b" ];
    environmentVariables = {
      OLLAMA_KEEP_ALIVE = "10m";
      OLLAMA_MAX_LOADED_MODELS = "1";
      OLLAMA_NUM_PARALLEL = "1";
    };
  };

  # Open WebUI — web chat interface with built-in local Whisper STT
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
