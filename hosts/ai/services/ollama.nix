{ config, lib, pkgs, ... }:
let
  inherit (config.custom.fleet) ai;
  models = lib.unique [ ai.model ai.codingModel ai.embeddingModel ];
  img = pkgs.containerImages.ollama;
  localApi = "http://127.0.0.1:${toString ai.endpoint.port}";

  configure = pkgs.writeShellApplication {
    name = "ollama-configure";
    runtimeInputs = [ pkgs.curl ];
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
        OLLAMA_MAX_LOADED_MODELS = toString (builtins.length models);
        # qwen35moe cannot do parallel requests; ollama forces this back to 1 and logs why.
        OLLAMA_NUM_PARALLEL = "1";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";       # halves KV VRAM (needs flash attention above)
        OLLAMA_CONTEXT_LENGTH = toString ai.contextLength;
        # Keeping all three resident OOM-killed ComfyUI, which needs ~30 GB on demand. ollama-configure
        # pins `ai.model` alone at -1, which later requests preserve; the rest reload in 5-8s.
        OLLAMA_KEEP_ALIVE = "10m";
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
