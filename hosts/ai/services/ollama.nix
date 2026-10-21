{ config, lib, pkgs, ... }:
let
  inherit (config.fleet) ai;
  models = lib.unique [ ai.model ai.codingModel ai.embeddingModel ai.uncensoredModel ];
  img = pkgs.containerImages.ollama;
  localApi = "http://127.0.0.1:${toString ai.endpoint.port}";

  configure = pkgs.writeShellApplication {
    name = "ollama-configure";
    runtimeInputs = [ pkgs.curl ];
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
      # ComfyUI peaks at ~54 GB, so only the pinned `ai.model` and one other may stay resident: two 27Bs
      # left ~1 GB spare. ollama-configure pins at -1, so it is evicted last; the rest reload in 5-8s.
      OLLAMA_MAX_LOADED_MODELS = "2";
      OLLAMA_KEEP_ALIVE = "10m";
      # qwen35moe cannot do parallel requests; ollama forces this back to 1 and logs why.
      OLLAMA_NUM_PARALLEL = "1";
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0";       # halves KV VRAM (needs flash attention above)
      OLLAMA_CONTEXT_LENGTH = toString ai.contextLength;
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

  systemd.services.ollama-configure = {
    description = "Pull the fleet's models into Ollama";
    wantedBy = [ "multi-user.target" ];
    after = [ "podman-ollama.service" ];
    requires = [ "podman-ollama.service" ];
    environment = {
      OLLAMA_API = localApi;
      DECLARED_MODELS = toString models;
      PINNED_MODEL = ai.model;
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      StateDirectory = "ollama-configure";
      TimeoutStartSec = "4h";   # First pull of a large model is bounded by the internet, not the box
      ExecStart = lib.getExe configure;
    };
  };
}
