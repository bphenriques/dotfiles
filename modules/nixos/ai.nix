# AI stack module — Ollama (LLM inference), Hermes (assistant runtime),
# NextChat (browser UI). Three components with explicit linking options;
# enable any subset on any host.
#
# Agnostic of homelab framework and microvm. Hosts pass URLs/key paths in
# as plain strings — wiring those URLs to whatever truth source the host
# uses (fleet config, homelab service registry, hardcoded IP) is the
# host's responsibility, not the module's.
{ config, lib, pkgs, inputs, ... }:
let
  cfg = config.custom.ai;
in
{
  # Upstream Hermes module declares `services.hermes-agent.*`. Loaded
  # unconditionally — option declarations are cheap, and it means hosts
  # don't need to remember the import alongside `custom.ai.hermes.enable`.
  imports = [ inputs.hermes-agent.nixosModules.default ];

  options.custom.ai = {
    model = lib.mkOption {
      type = lib.types.str;
      default = config.custom.fleet.ai.model;
      defaultText = lib.literalExpression "config.custom.fleet.ai.model";
      description = "LLM model identifier shared across Ollama load, Hermes default, and NextChat advertisement.";
    };

    # Cross-component values that MUST agree between Ollama and Hermes.
    # Hermes Agent's request shape includes context_length; if Ollama's
    # advertised window is below that, Hermes rejects the call at request
    # time ("below the minimum 64,000 required"). Same number wired into
    # `OLLAMA_CONTEXT_LENGTH`, `settings.model.context_length`, and
    # `settings.auxiliary.compression.context_length`.
    contextLength = lib.mkOption {
      type = lib.types.ints.positive;
      default = 65536;
      description = ''
        Token context window size, shared between Ollama and Hermes.
        Hermes enforces a 64K minimum; raising costs VRAM linearly.
      '';
    };

    maxTokens = lib.mkOption {
      type = lib.types.ints.positive;
      default = 8192;
      description = "Maximum output tokens per Hermes response.";
    };

    ollama = {
      enable = lib.mkEnableOption "Ollama local LLM inference";
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.ollama-cuda;
        defaultText = lib.literalExpression "pkgs.ollama-cuda";
        description = "Ollama package; switch to pkgs.ollama for CPU-only hosts.";
      };
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Bind address for the Ollama HTTP server.";
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 11434;
      };
      loadModels = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Models pre-pulled at activation. Include cfg.model.";
      };
      allowFromHosts = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Source IPs allowed through the firewall to the Ollama port (iptables backend).";
      };
    };

    hermes = {
      enable = lib.mkEnableOption "Hermes Agent runtime";
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "0.0.0.0";
        description = "Bind address for the Hermes API server.";
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8642;
      };
      ollamaUrl = lib.mkOption {
        type = lib.types.str;
        example = "http://192.168.1.121:11434/v1";
        description = "OpenAI-compatible base URL where Hermes finds the LLM.";
      };
      apiKeyFile = lib.mkOption {
        type = lib.types.path;
        description = ''
          Path to a file containing the rendered Hermes env (API_SERVER_KEY=…
          plus the static API_SERVER_* lines the module emits). Typically a
          sops.templates path.
        '';
      };
      vaultPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = "Local directory for the vault MCP server; null disables the vault tool.";
      };
      soul = lib.mkOption {
        type = lib.types.lines;
        description = "SOUL.md persona content.";
      };
      hermesHome = lib.mkOption {
        type = lib.types.path;
        default = "/var/lib/hermes";
        description = "Hermes runtime state directory.";
      };
    };

    chat = {
      enable = lib.mkEnableOption "NextChat browser UI";
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = ''
          Bind address inside the container. With --network=host (set
          unconditionally), HOSTNAME defaults to the host's hostname which
          can land on 127.0.0.2 via /etc/hosts and confuse a fronting
          reverse-proxy. Keep on 127.0.0.1.
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
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.ollama.enable {
      services.ollama = {
        enable = true;
        package = cfg.ollama.package;
        host = cfg.ollama.listenAddress;
        port = cfg.ollama.port;
        loadModels = cfg.ollama.loadModels;
        environmentVariables = {
          OLLAMA_KEEP_ALIVE = "2m";
          OLLAMA_MAX_LOADED_MODELS = "1";
          OLLAMA_NUM_PARALLEL = "1";
          OLLAMA_FLASH_ATTENTION = "1";
          OLLAMA_KV_CACHE_TYPE = "q8_0";  # halves VRAM on 8GB cards
          OLLAMA_CONTEXT_LENGTH = toString cfg.contextLength;
        };
      };

      # Source-IP-scoped firewall opens. iptables backend (NixOS default);
      # flip to extraInputRules if a host enables nftables.
      networking.firewall.extraCommands = lib.mkIf (cfg.ollama.allowFromHosts != [ ]) (
        lib.concatMapStringsSep "\n" (ip:
          "iptables -I nixos-fw -p tcp -s ${ip} --dport ${toString cfg.ollama.port} -j nixos-fw-accept"
        ) cfg.ollama.allowFromHosts
      );
    })

    (lib.mkIf cfg.hermes.enable {
      services.hermes-agent = {
        enable = true;
        addToSystemPackages = true;
        # "web" → FastAPI/Uvicorn (the API server); "pty" → embedded TUI chat.
        extraDependencyGroups = [ "web" "pty" ];
        environmentFiles = [ cfg.hermes.apiKeyFile ];

        mcpServers = {
          fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
          time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";
        } // lib.optionalAttrs (cfg.hermes.vaultPath != null) {
          vault = {
            command = "${pkgs.nodejs}/bin/npx";
            args = [ "-y" "@bitbonsai/mcpvault@0.11.0" (toString cfg.hermes.vaultPath) ];
          };
        };

        documents."SOUL.md" = cfg.hermes.soul;

        settings = {
          model = {
            default = cfg.model;
            provider = "custom";
            api_key = "ollama";
            base_url = cfg.hermes.ollamaUrl;
            context_length = cfg.contextLength;
            max_tokens = cfg.maxTokens;
          };
          auxiliary.compression.context_length = cfg.contextLength;
          dashboard.theme = "mono";
          display.theme = "mono";
          honcho.mode = "local";
          platform_toolsets = let
            tools = [ "fetch" "time" ] ++ lib.optional (cfg.hermes.vaultPath != null) "vault";
          in {
            cli = tools;
            api_server = tools;
          };
        };
      };

      systemd.services.hermes-agent.serviceConfig = {
        TimeoutStopSec = "210s";
        # The upstream module concatenates `environmentFiles` into
        # $HERMES_HOME/.env via an activation script — but a volume mount at
        # $hermesHome shadows that write, so it never appears. Inject the env
        # file as a real systemd EnvironmentFile= so the API_SERVER_* values
        # reach the process directly. Python-dotenv's default doesn't override
        # existing env vars, so the systemd path wins regardless.
        EnvironmentFile = [ cfg.hermes.apiKeyFile ];
        # Same volume-shadow issue for config.yaml. Rendered to /etc (in-VM
        # closure, no host coupling); ExecStartPre installs it into
        # $HERMES_HOME on every service start, post-mount. `+` prefix runs
        # as root so we can chown to hermes.
        ExecStartPre = [
          "+${pkgs.coreutils}/bin/install -D -m 0640 -o hermes -g hermes /etc/hermes-agent/config.yaml ${cfg.hermes.hermesHome}/.hermes/config.yaml"
        ];
      };

      environment.etc."hermes-agent/config.yaml".source =
        pkgs.writeText "hermes-config.yaml"
          (builtins.toJSON config.services.hermes-agent.settings);
    })

    (lib.mkIf cfg.chat.enable (let
      img = pkgs.containerImages.nextchat;
    in {
      virtualisation.oci-containers.containers.nextchat = {
        image = "${img.image}:v${img.version}";
        autoStart = true;
        environment = {
          HOSTNAME = cfg.chat.listenAddress;
          PORT = toString cfg.chat.port;
          BASE_URL = cfg.chat.hermesUrl;
          # Hermes presents itself as model "hermes-agent" regardless of
          # the underlying LLM. NextChat talks to Hermes, not Ollama, so
          # this is a constant — `cfg.model` (the Ollama identifier)
          # doesn't apply here.
          CUSTOM_MODELS = "-all,+hermes-agent";
          DEFAULT_MODEL = "hermes-agent";
          # Minimal/locked-down UX — single backend, no per-user overrides.
          HIDE_USER_API_KEY = "1";
          HIDE_BALANCE_QUERY = "1";  # Hermes has no balance endpoint
          DISABLE_FAST_LINK = "1";
          DISABLE_GPT4 = "1";
          WHITE_WEBDAV_ENDPOINTS = "";
        };
        environmentFiles = [ cfg.chat.hermesApiKeyEnvFile ];
        extraOptions = [
          "--network=host"
          "--memory=1g"  # 512m was tight; Next.js standalone can OOM-restart under sustained sessions
        ];
      };
    }))
  ];
}
