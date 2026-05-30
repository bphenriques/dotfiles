# NextChat — browser UI fronting the Hermes API. The container runtime, env
# wiring, and resource limits live here; homelab framework registration and
# secret plumbing stay in the host config that enables this.
{ config, lib, pkgs, ... }:
let
  cfg = config.custom.ai.nextchat;
in
{
  options.custom.ai.nextchat = {
    enable = lib.mkEnableOption "NextChat browser UI";
    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = ''
        Bind address inside the container. With `--network=host` (set
        unconditionally), HOSTNAME defaults to the host's hostname which can
        land on 127.0.0.2 via /etc/hosts and confuse a fronting reverse-proxy.
        Keep on 127.0.0.1.
      '';
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 3210;
    };
    hermesUrl = lib.mkOption {
      type = lib.types.str;
      description = "Hermes API base URL (used as NextChat's BASE_URL).";
    };
    hermesApiKeyEnvFile = lib.mkOption {
      type = lib.types.path;
      description = "Env file containing OPENAI_API_KEY=<hermes-api-key>. Typically a sops.templates path.";
    };
  };

  config = lib.mkIf cfg.enable (let
    img = pkgs.containerImages.nextchat;
  in {
    virtualisation.oci-containers.containers.nextchat = {
      image = "${img.image}:v${img.version}";
      autoStart = true;
      environment = {
        HOSTNAME = cfg.listenAddress;
        PORT = toString cfg.port;
        BASE_URL = cfg.hermesUrl;
        # Hermes presents itself as model "hermes-agent" regardless of the
        # underlying LLM. NextChat talks to Hermes, not Ollama, so the model
        # advertised here is a constant — `custom.fleet.ai.model` (the Ollama
        # identifier) doesn't apply.
        CUSTOM_MODELS = "-all,+hermes-agent";
        DEFAULT_MODEL = "hermes-agent";
        # Minimal/locked-down UX — single backend, no per-user overrides.
        HIDE_USER_API_KEY = "1";
        HIDE_BALANCE_QUERY = "1";  # Hermes has no balance endpoint
        DISABLE_FAST_LINK = "1";
        DISABLE_GPT4 = "1";
        WHITE_WEBDAV_ENDPOINTS = "";
      };
      environmentFiles = [ cfg.hermesApiKeyEnvFile ];
      extraOptions = [
        "--network=host"
        "--memory=1g"  # 512m was tight; Next.js standalone can OOM-restart under sustained sessions
      ];
    };
  });
}
