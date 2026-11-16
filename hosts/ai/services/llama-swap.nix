{ config, lib, pkgs, ... }:
let
  inherit (config.fleet) ai;
  files = import ./models.nix;
  modelsDir = "/var/lib/llama-swap/models";
  img = pkgs.containerImages.gufo;
  podman = lib.getExe config.virtualisation.podman.package;

  gufo = id: args: {
    cmd = lib.concatStringsSep " " ([
      podman "run" "--rm" "--replace" "--name" id
      "--network=host" # so llama-swap reaches the loopback port Gufo binds
      "--cap-drop=ALL"
      "--security-opt=no-new-privileges"
      "--device=/dev/kfd"
      "--device=/dev/dri"
      "--volume=${modelsDir}/${id}:/model:ro"
      "${img.image}:${img.tag}"
      "gufo" "serve"
    ] ++ args ++ [ "--served-model-name" id "--host" "127.0.0.1" "--port" "\${PORT}" ]);
    cmdStop = "${podman} stop --time 30 ${id}";
    checkEndpoint = "/v1/models";
  };

  settings = {
    healthCheckTimeout = 600; # Flash-Next reads ~110 GB when the page cache is cold
    models = {
      "qwen3.8-flash-next" = gufo "qwen3.8-flash-next" [
        "llm"
        "--model" "/model/UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00001-of-00004.gguf"
        "--mmproj" "/model/mmproj-BF16.gguf"
        "--speculative" "mtp"
        "--mtp-model" "/model/MTP/mtp-Qwen3.8-Flash-Next-shared-Q8_0.gguf"
        "--context" (toString ai.contextLength)
        "--sessions" "2"                   # coding and the assistant side by side
        "--reasoning-effort" "low"         # Gufo defaults to thinking hard; omp asks for more per request
        "--cache-ram-bytes" "8589934592"   # prompt cache in RAM, auto would take up to 32 GiB
      ];
      "qwen3.8-27b-uncensored" = gufo "qwen3.8-27b-uncensored" [
        "llm"
        "--model" "/model/Qwen3.8-27B-Uncensored-Q4_K_M.gguf"
        "--mmproj" "/model/mmproj-Qwen3.8-27B-Uncensored-f16.gguf"
        "--speculative" "dflash2"
        "--dflash-model" "/model/Qwen3.8-27B-DFlash2-Q4_K_M.gguf"
        "--context" "32768"
        "--reasoning-effort" "low"
      ];
      "qwen-image-2.1-turbo" = gufo "qwen-image-2.1-turbo" [ "image" "--model" "/model" ];
      "embeddinggemma-2" = gufo "embeddinggemma-2" [ "embedding" "--model" "/model" ];
    };

    # The only combinations allowed to share the iGPU (README "Memory"); anything else swaps.
    routing.router = {
      use = "matrix";
      settings.matrix = {
        vars = {
          flash = "qwen3.8-flash-next";
          uncensored = "qwen3.8-27b-uncensored";
          image = "qwen-image-2.1-turbo";
          embed = "embeddinggemma-2";
        };
        sets = {
          text = "flash & embed";
          images = "uncensored & image & embed";
        };
      };
    };

    hooks.on_startup.preload = [ ai.model ];
  };

  configFile = (pkgs.formats.yaml { }).generate "llama-swap.yaml" settings;

  configure = pkgs.writeShellApplication {
    name = "llama-swap-configure";
    runtimeInputs = [ pkgs.curl pkgs.jq ];
    text = builtins.readFile ./llama-swap-configure.sh;
  };
in
{
  assertions = [
    {
      assertion = lib.attrNames files == lib.attrNames settings.models;
      message = "llama-swap: models.nix and the served models must list the same ids";
    }
    {
      assertion = lib.all (m: settings.models ? ${m}) [ ai.model ai.uncensoredModel ai.imageModel ai.embeddingModel ];
      message = "llama-swap: every model in fleet.ai must be served";
    }
  ];

  # The boot preload raced amdgpu: /dev/kfd exists before the GPU joins it, the render node only after.
  services.udev.extraRules = ''
    KERNEL=="renderD128", TAG+="systemd"
  '';

  systemd.services.llama-swap = {
    description = "OpenAI-compatible endpoint that swaps Gufo models on the iGPU";
    wantedBy = [ "multi-user.target" ];
    requires = [ "dev-dri-renderD128.device" ];
    after = [ "dev-dri-renderD128.device" ];
    serviceConfig = {
      ExecStart = "${lib.getExe pkgs.llama-swap} --config ${configFile} --listen 0.0.0.0:${toString ai.endpoint.port}";
      Restart = "on-failure";
      RestartSec = "10s";
    };
  };

  systemd.services.llama-swap-configure = {
    description = "Download the models llama-swap serves";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    environment = {
      MODELS_DIR = modelsDir;
      MANIFEST = pkgs.writeText "llama-swap-models.json" (builtins.toJSON (lib.concatLists (lib.mapAttrsToList
        (id: map (f: f // { path = "${id}/${f.path}"; })) files)));
    };
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      StateDirectory = "llama-swap";
      TimeoutStartSec = "6h"; # a first download is bounded by the internet, not the box
      ExecStart = lib.getExe configure;
    };
  };
}
