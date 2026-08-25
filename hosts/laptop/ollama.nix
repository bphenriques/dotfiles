{ config, pkgs, ... }:
let
  fleet = config.custom.fleet;
in
{
  # TODO: Move to a docker
  services.ollama = {
    enable = true;
    package = pkgs.ollama-cuda;
    host = "0.0.0.0";
    port = 11434;
    loadModels = [ fleet.ai.model ] ++ fleet.ai.extraModels;
    environmentVariables = {
      OLLAMA_MAX_LOADED_MODELS = "1";
      OLLAMA_NUM_PARALLEL = "1";
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0";       # halves KV VRAM (needs flash attention above)
      OLLAMA_CONTEXT_LENGTH = "65536";     # Hermes requires >=64K
      OLLAMA_KEEP_ALIVE = "1h";
    };
  };
}
