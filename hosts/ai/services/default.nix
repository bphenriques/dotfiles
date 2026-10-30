{ lib, ... }:
{
  imports = [
    ./comfyui.nix
    ./monitoring.nix
    ./ollama.nix
    ./whisper.nix
  ];

  virtualisation = {
    podman.enable = true;
    podman.autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" ]; # image tags pile up on every bump; only running containers keep theirs
    };
    oci-containers.backend = "podman";
  };

  systemd.services = lib.genAttrs [ "podman-ollama" "podman-comfyui" ] (_: {
    serviceConfig = {
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  });
}
