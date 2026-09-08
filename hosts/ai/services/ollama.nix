{ config, lib, pkgs, ... }:
let
  inherit (config.custom.fleet) ai;
  models = [ ai.model ai.embeddingModel ] ++ ai.extraModels;
  img = pkgs.containerImages.ollama;
  localApi = "http://127.0.0.1:${toString ai.endpoint.port}";

  configure = pkgs.writeShellApplication {
    name = "ollama-configure";
    runtimeInputs = [ pkgs.curl pkgs.jq ];
    text = builtins.readFile ./ollama-configure.sh;
  };
in
{
  virtualisation = {
    podman.enable = true;
    podman.autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" ]; # image tags pile up on every bump; only running containers keep theirs
    };
    oci-containers.backend = "podman";

    oci-containers.containers.ollama = {
      image = "${img.image}:${img.version}-rocm";
      autoStart = true;
      volumes = [ "ollama:/root/.ollama" ];
      environment = {
        OLLAMA_HOST = "0.0.0.0:${toString ai.endpoint.port}";
        # `model` and `codingModel` share a parent_model, so they are one runner, not two.
        OLLAMA_MAX_LOADED_MODELS = "3";
        # qwen35moe cannot do parallel requests; ollama forces this back to 1 and logs why.
        OLLAMA_NUM_PARALLEL = "1";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";       # halves KV VRAM (needs flash attention above)
        OLLAMA_CONTEXT_LENGTH = toString ai.contextLength;
        # ~25 GB of 112 GiB GTT, so an expiry only buys a 5-8s cold load after every idle spell.
        OLLAMA_KEEP_ALIVE = "-1";
      };
      # Host networking, not a published port: netavark DNATs published ports in nat-prerouting,
      # which runs before the input hook, so ../firewall.nix would never see the traffic.
      # These are flags rather than containers.conf keys so a deploy reasserts them via ExecStart.
      extraOptions = [
        "--network=host"
        "--cap-drop=ALL"
        "--security-opt=no-new-privileges"
        "--device=/dev/kfd"
        "--device=/dev/dri"
      ];
    };
  };

  systemd.services.podman-ollama.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };

  systemd.services.ollama-configure = {
    description = "Pull the fleet's models into Ollama";
    wantedBy = [ "multi-user.target" ];
    after = [ "podman-ollama.service" ];
    requires = [ "podman-ollama.service" ];
    environment = {
      OLLAMA_API = localApi;
      OLLAMA_MODELS = toString models;
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "4h";   # First pull of a large model is bounded by the internet, not the box
      ExecStart = lib.getExe configure;
    };
  };
}
