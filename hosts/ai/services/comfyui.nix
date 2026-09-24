{ config, lib, pkgs, ... }:
let
  inherit (config.custom.fleet.ai) imageEndpoint;
  img = pkgs.containerImages.comfyui;

  # Taken from the image's own `start_comfy_ui` alias. `--disable-mmap` is the load-bearing one:
  # mmap above 64GB is pathologically slow on gfx1151. `--listen` is ours, the alias binds loopback.
  args = [
    "--listen" "0.0.0.0"
    "--port" (toString imageEndpoint.port)
    "--output-directory" "/opt/comfy-home/comfy-outputs"
    "--disable-mmap"
    "--gpu-only"
    "--disable-smart-memory"
    "--cache-none"
    "--bf16-vae"
  ];
in
{
  virtualisation.oci-containers.containers.comfyui = {
    image = "${img.image}:${img.version}";
    autoStart = true;

    # Not /root: Fedora ships it mode 0550, unwritable once --cap-drop=ALL removes CAP_DAC_OVERRIDE.
    volumes = [ "comfyui:/opt/comfy-home" ];

    environment = {
      # podman leaves HOME unset, and set_extra_paths.sh expands it under `set -u`.
      HOME = "/opt/comfy-home";
      # The image exports these from /etc/profile.d, which only a login shell would ever read.
      TORCH_ROCM_AOTRITON_ENABLE_EXPERIMENTAL = "1";
      TORCH_BLAS_PREFER_HIPBLASLT = "1";
    };

    entrypoint = "/bin/bash";
    cmd = [
      "-c"
      "/opt/set_extra_paths.sh && cd /opt/ComfyUI && exec /opt/venv/bin/python main.py ${lib.escapeShellArgs args}"
    ];

    # Host networking rather than a published port, for the reason ollama.nix spells out.
    extraOptions = [
      "--network=host"
      "--cap-drop=ALL"
      "--security-opt=no-new-privileges"
      # ROCm's ioctls trip podman's default seccomp profile; upstream ships this flag for that reason.
      "--security-opt=seccomp=unconfined"
      "--device=/dev/kfd"
      "--device=/dev/dri"
    ];
  };
}
