{ config, lib, pkgs, ... }:
let
  inherit (config.custom.fleet) ai;
  models = [ ai.model ] ++ ai.extraModels;
  img = pkgs.containerImages.ollama;
  localApi = "http://127.0.0.1:${toString ai.endpoint.port}";
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
    containers.containersConf.settings.containers = {
      no_new_privileges = true;
      default_capabilities = [ ];  # verified: ollama binds 11434 and drives the GPU with none
    };

    # Binds every interface because the LAN address is DHCP-assigned; ../firewall.nix is the control.
    oci-containers.containers.ollama = {
      image = "${img.image}:${img.version}-rocm";
      autoStart = true;
      ports = [ "${toString ai.endpoint.port}:11434" ];
      volumes = [ "ollama:/root/.ollama" ];
      environment = {
        OLLAMA_HOST = "0.0.0.0:11434";
        OLLAMA_MAX_LOADED_MODELS = "1";
        OLLAMA_NUM_PARALLEL = "1";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";       # halves KV VRAM (needs flash attention above)
        OLLAMA_CONTEXT_LENGTH = "65536";     # Hermes requires >=64K
        OLLAMA_KEEP_ALIVE = "1h";
      };
      # /dev/kfd and the render node are mode 0666 here, so no group juggling is needed.
      extraOptions = [
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
    path = [ pkgs.curl ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "4h";   # First pull of a large model is bounded by the internet, not the box
    };
    script = ''
      until curl -sf ${localApi}/api/version >/dev/null; do sleep 2; done
      ${lib.concatMapStringsSep "\n" (m: ''
        echo "pulling ${m}"
        # /api/pull answers 200 even for a bad tag and reports failure inside the stream, so the
        # last line is the only verdict.
        verdict=$(curl -sf ${localApi}/api/pull -d '{"model":"${m}"}' | tail -1)
        case "$verdict" in
          *'"error"'*) echo "pull of ${m} failed: $verdict" >&2; exit 1 ;;
        esac
      '') models}
    '';
  };
}
