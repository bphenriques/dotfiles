{ config, lib, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.voice-assistant;
  ntfyCfg = config.custom.homelab.services.ntfy;
  whisperCpp = pkgs.whisper-cpp.override { withFFmpegSupport = true; };
  pythonWithWyoming = pkgs.python3.withPackages (ps: [ ps.wyoming ]);
  # Gemma 4 model type requires transformers >= 5.4.0 (added after nixpkgs 5.3.0)
  transformersGemma4 = pkgs.python3Packages.transformers.overridePythonAttrs (old: rec {
    version = "5.5.0";
    src = pkgs.fetchFromGitHub {
      owner = "huggingface";
      repo = "transformers";
      tag = "v${version}";
      hash = "sha256-2fOCORAsQDKxp6EPe1OHysPWb/q168z6sCYg89tRXdU=";
    };
  });
  pythonWithGemma4 = pkgs.python3.withPackages (ps: [ ps.torch ps.torchvision transformersGemma4 ps.accelerate ps.soundfile ps.librosa ps.bitsandbytes ]);

  pythonWithParakeet = pkgs.python3.withPackages (ps: [ ps.onnx-asr ps.onnxruntime ps.huggingface-hub ]);

  parakeetHost = "127.0.0.1";
  parakeetPort = 5092;
  parakeetUrl = "http://${parakeetHost}:${toString parakeetPort}/v1/audio/transcriptions";

  whisperHost = "127.0.0.1";
  whisperPort = 8178;
  whisperUrl = "http://${whisperHost}:${toString whisperPort}/inference";

  modelDir = "/var/lib/whisper";
  modelSrc = "${modelDir}/ggml-small.bin";
  modelFile = "${modelDir}/ggml-small-q8_0.bin";
  modelUrl = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-small.bin";

  whisperEnHost = "127.0.0.1";
  whisperEnPort = 8181;
  whisperEnUrl = "http://${whisperEnHost}:${toString whisperEnPort}/inference";

  modelEnSrc = "${modelDir}/ggml-small.en.bin";
  modelEnFile = "${modelDir}/ggml-small.en-q8_0.bin";
  modelEnUrl = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-small.en.bin";

  gemma4Host = "127.0.0.1";
  gemma4Port = 8180;
  gemma4Url = "http://${gemma4Host}:${toString gemma4Port}/";

  ollamaModel = "gemma3:4b";
in
{
  custom.homelab.services.voice-assistant = {
    displayName = "Voice Assistant";
    metadata.description = "Voice-to-text";
    metadata.version = whisperCpp.version;
    metadata.homepage = "https://github.com/ggml-org/whisper.cpp";
    metadata.category = "Productivity";
    port = 8179;
    subdomain = "voice";
    forwardAuth.enable = true;
    healthcheck.path = "/health";
    integrations.ntfy.enable = true;
    integrations.ntfy.topic = "voice-assistant";
  };

  # Internal STT service — not exposed via Traefik
  systemd.services.whisper-server = {
    description = "Whisper.cpp STT Server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.ffmpeg-headless ];
    serviceConfig = {
      ExecStartPre = "${pkgs.writeShellScript "whisper-download-model" ''
        if [ ! -f "${modelSrc}" ]; then
          echo "Downloading whisper small model..."
          ${pkgs.curl}/bin/curl -fSL -o "${modelSrc}.tmp" "${modelUrl}"
          mv "${modelSrc}.tmp" "${modelSrc}"
          echo "Download complete"
        fi
        if [ ! -f "${modelFile}" ]; then
          echo "Quantizing to Q8..."
          ${whisperCpp}/bin/whisper-quantize "${modelSrc}" "${modelFile}" q8_0
          echo "Quantization complete"
        fi
      ''}";
      ExecStart = builtins.concatStringsSep " " [
        "${whisperCpp}/bin/whisper-server"
        "--host ${whisperHost}"
        "--port ${toString whisperPort}"
        "--model ${modelFile}"
        "--language auto"
        "--threads 4"
        "--convert"
        # Greedy decoding: skip retries and unused output
        "--no-timestamps"
        "--no-fallback"
      ];
      DynamicUser = true;
      StateDirectory = "whisper";
      WorkingDirectory = "/var/lib/whisper";
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
    };
  };

  # Whisper small.en (English-only) — for A/B comparison against multilingual small
  # Shares StateDirectory with whisper-server (both use /var/lib/whisper)
  systemd.services.whisper-en-server = {
    description = "Whisper.cpp STT Server (English-only)";
    after = [ "network.target" "whisper-server.service" ];  # after whisper-server so model dir exists
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.ffmpeg-headless ];
    serviceConfig = {
      ExecStartPre = "${pkgs.writeShellScript "whisper-download-model-en" ''
        if [ ! -f "${modelEnSrc}" ]; then
          echo "Downloading whisper small.en model..."
          ${pkgs.curl}/bin/curl -fSL -o "${modelEnSrc}.tmp" "${modelEnUrl}"
          mv "${modelEnSrc}.tmp" "${modelEnSrc}"
          echo "Download complete"
        fi
        if [ ! -f "${modelEnFile}" ]; then
          echo "Quantizing to Q8..."
          ${whisperCpp}/bin/whisper-quantize "${modelEnSrc}" "${modelEnFile}" q8_0
          echo "Quantization complete"
        fi
      ''}";
      ExecStart = builtins.concatStringsSep " " [
        "${whisperCpp}/bin/whisper-server"
        "--host ${whisperEnHost}"
        "--port ${toString whisperEnPort}"
        "--model ${modelEnFile}"
        "--language en"
        "--threads 4"
        "--convert"
        "--no-timestamps"
        "--no-fallback"
      ];
      DynamicUser = true;
      StateDirectory = "whisper";
      WorkingDirectory = "/var/lib/whisper";
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
    };
  };

  # Wyoming faster-whisper — for benchmarking against whisper-server
  # To test: switch WHISPER_URL in voice-assistant env to use Wyoming client instead
  # For now, just runs alongside for direct CLI benchmarking
  services.wyoming.faster-whisper.servers.benchmark = {
    enable = true;
    uri = "tcp://127.0.0.1:10300";
    model = "small-int8";
    language = "en";
    device = "cpu";
    beamSize = 2;
  };

  # Gemma 4 E4B — native audio LLM for A/B/C benchmarking
  # Validation setup via HuggingFace Transformers until Ollama supports audio input
  systemd.services.gemma4-server = {
    description = "Gemma 4 E4B Audio Transcription Server";
    after = [ "network.target" ];
    wantedBy = lib.mkForce [];
    restartIfChanged = false;
    path = [ pkgs.ffmpeg-headless ];
    environment = {
      LISTEN_HOST = gemma4Host;
      LISTEN_PORT = toString gemma4Port;
      HF_HOME = "/var/lib/gemma4";
      OMP_NUM_THREADS = "4";
      MKL_NUM_THREADS = "4";
      OMP_WAIT_POLICY = "PASSIVE";
      NUMBA_CACHE_DIR = "/var/lib/gemma4/numba-cache";
    };
    serviceConfig = {
      ExecStart = "${pythonWithGemma4}/bin/python3 ${./gemma4-server.py}";
      DynamicUser = true;
      StateDirectory = "gemma4";
      WorkingDirectory = "/var/lib/gemma4";
      MemoryHigh = "10G";
      MemoryMax = "12G";
      MemorySwapMax = "0";
      CPUQuota = "150%";
      CPUWeight = 10;
      IOWeight = 10;
      Nice = 10;
      OOMScoreAdjust = 500;
      Restart = "no";
      TimeoutStartSec = "15min";  # model download + load on first start
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
    };
  };

  # Parakeet TDT 0.6B v3 — NVIDIA NeMo ONNX model, fast CPU-only STT
  systemd.services.parakeet-server = {
    description = "Parakeet TDT 0.6B v3 STT Server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.ffmpeg-headless ];
    environment = {
      LISTEN_HOST = parakeetHost;
      LISTEN_PORT = toString parakeetPort;
      HF_HOME = "/var/lib/parakeet";
      HF_HUB_CACHE = "/var/lib/parakeet";
    };
    serviceConfig = {
      ExecStart = "${pythonWithParakeet}/bin/python3 ${./parakeet-server.py}";
      DynamicUser = true;
      StateDirectory = "parakeet";
      WorkingDirectory = "/var/lib/parakeet";
      MemoryHigh = "2G";
      MemoryMax = "3G";
      MemorySwapMax = "0";
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
      TimeoutStartSec = "10min";  # model download on first start
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
    };
  };

  # Ollama — local LLM for transcript enhancement and image understanding
  services.ollama = {
    enable = true;
    host = "127.0.0.1";
    port = 11434;
    loadModels = [ ollamaModel ];
  };
  systemd.services.ollama.serviceConfig = {
    MemoryHigh = "6G";
    MemoryMax = "8G";
    MemorySwapMax = "0";
    CPUQuota = "200%";  # max 2 of 4 cores; prevents thermal shutdown
  };
  services.ollama.environmentVariables = {
    OLLAMA_LOAD_TIMEOUT = "10m";  # vision projector load can be slow on CPU
  };

  # HTTP proxy: audio → whisper-server → ntfy
  systemd.services.voice-assistant = {
    description = "Voice Assistant";
    after = [ "whisper-server.service" "whisper-en-server.service" "ntfy-configure.service" "ollama.service" "parakeet-server.service" ];
    wants = [ "whisper-server.service" "whisper-en-server.service" "ntfy-configure.service" "ollama.service" "parakeet-server.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      LISTEN_HOST = serviceCfg.host;
      LISTEN_PORT = toString serviceCfg.port;
      WHISPER_URL = whisperUrl;
      WHISPER_EN_URL = whisperEnUrl;
      NTFY_URL = "${ntfyCfg.url}/${serviceCfg.integrations.ntfy.topic}";
      NTFY_TOKEN_FILE = serviceCfg.integrations.ntfy.tokenFile;
      CURL_PATH = "${pkgs.curl}/bin/curl";
      WYOMING_HOST = "127.0.0.1";
      WYOMING_PORT = "10300";
      WYOMING_SCRIPT = "${./wyoming-transcribe.py}";
      PYTHON_PATH = "${pythonWithWyoming}/bin/python3";
      GEMMA4_URL = "";  # gemma4-server is disabled; set to gemma4Url when re-enabled
      OLLAMA_URL = "http://127.0.0.1:11434";
      OLLAMA_MODEL = ollamaModel;
      PARAKEET_URL = parakeetUrl;
    };
    path = [ pkgs.ffmpeg-headless ];
    serviceConfig = {
      ExecStart = "${pythonWithWyoming}/bin/python3 ${./server.py}";
      DynamicUser = true;
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      BindReadOnlyPaths = [ serviceCfg.integrations.ntfy.tokenFile ];
    };
  };
}
