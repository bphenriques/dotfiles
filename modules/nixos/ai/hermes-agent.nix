# Hermes Agent runtime — the assistant runtime fronting the LLM backend(s).
# Imports the upstream module unconditionally (option declarations are cheap)
# so hosts don't have to remember it alongside `custom.ai.hermes-agent.enable`.
{ config, lib, pkgs, inputs, ... }:
let
  cfg = config.custom.ai.hermes-agent;
  fleetAi = config.custom.fleet.ai;
in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  options.custom.ai.hermes-agent = {
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
        plus the static API_SERVER_* lines this module emits). Typically a
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

  config = lib.mkIf cfg.enable {
    services.hermes-agent = {
      enable = true;
      addToSystemPackages = true;
      # "web" → FastAPI/Uvicorn (the API server); "pty" → embedded TUI chat.
      extraDependencyGroups = [ "web" "pty" ];
      environmentFiles = [ cfg.apiKeyFile ];

      mcpServers = {
        fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
        time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";
      } // lib.optionalAttrs (cfg.vaultPath != null) {
        vault = {
          command = "${pkgs.nodejs}/bin/npx";
          args = [ "-y" "@bitbonsai/mcpvault@0.11.0" (toString cfg.vaultPath) ];
        };
      };

      documents."SOUL.md" = cfg.soul;

      settings = {
        model = {
          default = fleetAi.model;
          provider = "custom";
          api_key = "ollama";
          base_url = cfg.ollamaUrl;
          context_length = fleetAi.contextLength;
          max_tokens = fleetAi.maxTokens;
        };
        auxiliary.compression.context_length = fleetAi.contextLength;
        dashboard.theme = "mono";
        display.theme = "mono";
        honcho.mode = "local";
        platform_toolsets = let
          tools = [ "fetch" "time" ] ++ lib.optional (cfg.vaultPath != null) "vault";
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
      EnvironmentFile = [ cfg.apiKeyFile ];
      # Same volume-shadow issue for config.yaml. Rendered to /etc (in-VM
      # closure, no host coupling); ExecStartPre installs it into $HERMES_HOME
      # on every service start, post-mount. `+` prefix runs as root so we can
      # chown to hermes.
      ExecStartPre = [
        "+${pkgs.coreutils}/bin/install -D -m 0640 -o hermes -g hermes /etc/hermes-agent/config.yaml ${cfg.hermesHome}/.hermes/config.yaml"
      ];
    };

    environment.etc."hermes-agent/config.yaml".source =
      pkgs.writeText "hermes-config.yaml"
        (builtins.toJSON config.services.hermes-agent.settings);
  };
}
