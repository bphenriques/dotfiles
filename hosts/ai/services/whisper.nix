{ config, lib, pkgs, ... }:
let
  inherit (config.fleet.ai) speechEndpoint;
  model = pkgs.fetchurl {
    url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-large-v3-turbo-q5_0.bin";
    hash = "sha256-OUIhcJzVrR9AxG5gMcphvOiJMebgiMGIKUxtWlX/p+I=";
  };
  vadModel = pkgs.fetchurl {
    url = "https://huggingface.co/ggml-org/whisper-vad/resolve/main/ggml-silero-v5.1.2.bin";
    hash = "sha256-KZQNmNQrkfvQXOSJ8+z3xy8KQvAn5IdZGaKPtMBOos8=";
  };
in
{
  # Vulkan places the ~810 MiB model in the 2 GiB VRAM carve-out, which ROCm leaves nearly empty, not the GTT pool Ollama and ComfyUI share.
  systemd.services.whisper = {
    description = "Speech-to-text API (whisper.cpp on the iGPU)";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    serviceConfig = {
      ExecStart = lib.escapeShellArgs [
        (lib.getExe' pkgs.whisper-cpp-vulkan "whisper-server")
        "--model" model
        "--vad" "--vad-model" vadModel  # skips music-only stretches, where whisper otherwise invents words
        "--language" "auto"
        "--convert" "--tmp-dir" "/tmp"
        "--host" "0.0.0.0"
        "--port" (toString speechEndpoint.port)
      ];
      DynamicUser = true;
      SupplementaryGroups = [ "render" ];
      Restart = "on-failure";
      RestartSec = "10s";
    };
  };
}
