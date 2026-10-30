{ config, lib, pkgs, ... }:
let
  inherit (config.fleet) ai;
  models = lib.unique [ ai.model ai.codingModel ai.embeddingModel ai.uncensoredModel ];
  # The registry cannot serve these (sharded GGUF plus a DRAFT head), so ollama-configure creates them.
  built = {
    "qwen3.8-flash-next" = {
      modelfile = ./qwen3.8-flash-next.Modelfile;
      files = map (f: "https://huggingface.co/unsloth/Qwen3.8-Flash-Next-GGUF/resolve/766911a6b7369840a91dbcd95f9f997acaab6cd6/${f}") [
        "UD-IQ4_XS/Qwen3.8-Flash-Next-UD-IQ4_XS-00001-of-00003.gguf"
        "UD-IQ4_XS/Qwen3.8-Flash-Next-UD-IQ4_XS-00002-of-00003.gguf"
        "UD-IQ4_XS/Qwen3.8-Flash-Next-UD-IQ4_XS-00003-of-00003.gguf"
        "mtp-Qwen3.8-Flash-Next-Q8_0.gguf"
      ];
    };
  };
  img = pkgs.containerImages.ollama;
  localApi = "http://127.0.0.1:${toString ai.endpoint.port}";

  configure = pkgs.writeShellApplication {
    name = "ollama-configure";
    runtimeInputs = [ pkgs.curl pkgs.jq pkgs.podman ];
    text = builtins.readFile ./ollama-configure.sh;
  };
in
{
  virtualisation.oci-containers.containers.ollama = {
    image = "${img.image}:${img.tag}";
    autoStart = true;
    volumes = [ "ollama:/root/.ollama" ];
    environment = {
      OLLAMA_HOST = "0.0.0.0:${toString ai.endpoint.port}";
      # The pinned `ai.model` plus one other. ComfyUI (~54 GB) does not fit beside `codingModel` (~65 GB).
      # ollama-configure pins at -1, so it is evicted last.
      OLLAMA_MAX_LOADED_MODELS = "2";
      OLLAMA_KEEP_ALIVE = "10m";
      # qwen35moe cannot do parallel requests; ollama forces this back to 1 and logs why.
      OLLAMA_NUM_PARALLEL = "1";
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0";       # halves KV VRAM (needs flash attention above)
      OLLAMA_CONTEXT_LENGTH = toString ai.contextLength;
      OLLAMA_NO_CLOUD = "1";               # remote inference and web search would ship prompts to ollama.com
    };
    # Host networking: netavark DNATs published ports before the input hook, out of ../firewall.nix's reach.
    extraOptions = [
      "--network=host"
      "--cap-drop=ALL"
      "--security-opt=no-new-privileges"
      "--device=/dev/kfd"
      "--device=/dev/dri"
    ];
  };

  assertions = [{
    assertion = lib.all (m: lib.elem m models) (lib.attrNames built);
    message = "ollama: every built model must be declared in fleet.ai";
  }];

  systemd.services.ollama-configure = {
    description = "Pull or create the fleet's models in Ollama";
    # Re-runs on every container start, crash restarts included: a restarted Ollama has forgotten the pin.
    wantedBy = [ "podman-ollama.service" ];
    after = [ "podman-ollama.service" ];
    bindsTo = [ "podman-ollama.service" ];
    environment = {
      OLLAMA_API = localApi;
      DECLARED_MODELS = toString models;
      BUILT_MODELS = pkgs.writeText "ollama-built-models.json" (builtins.toJSON built);
      OLLAMA_IMAGE = "${img.image}:${img.tag}";
      PINNED_MODEL = ai.model;
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      StateDirectory = "ollama-configure";
      CacheDirectory = "ollama-configure";
      TimeoutStartSec = "4h";   # First pull of a large model is bounded by the internet, not the box
      ExecStart = lib.getExe configure;
    };
  };
}
