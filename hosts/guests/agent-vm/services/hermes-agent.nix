{ config, lib, pkgs, fleet, agentVm, inputs, ... }:
let
  endpoint = "http://${fleet.ai.endpoint.host}:${toString fleet.ai.endpoint.port}/v1";
  aiEndpoint = {
    api_key = "ollama";   # dummy; Ollama needs no auth
    base_url = endpoint;
  };
in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  # The module deep-merges `settings` into the stateful config.yaml and never drops a key, so
  # anything removed from Nix lingers forever. Installing it verbatim keeps the file declarative.
  services.hermes-agent.configFile =
    pkgs.writeText "hermes-config.yaml" (builtins.toJSON config.services.hermes-agent.settings);

  services.hermes-agent = {
    enable = true;
    stateDir = agentVm.stateRoot;
    hermesHomeFiles."SOUL.md" =
      builtins.replaceStrings [ "@vaultRoot@" ] [ agentVm.vaultRoot ] (builtins.readFile ../SOUL.md);
    addToSystemPackages = true;   # `hermes` CLI in the guest, for debugging against the live state

    # mcpvault's own bin is `#!/usr/bin/env node`, so npx alone is not enough: the unit PATH needs node.
    extraPackages = [ pkgs.nodejs ];

    mcpServers = {
      fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
      time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";

      # uvx resolves the package from PyPI on first start (guest has internet), same as vault below.
      web-search = {
        command = "${pkgs.uv}/bin/uvx";
        args = [ "duckduckgo-mcp-server@0.7.0" ];

        # uv otherwise fetches a generic-linux CPython that NixOS cannot exec.
        env = {
          UV_PYTHON_DOWNLOADS = "never";
          UV_PYTHON = "${pkgs.python3}/bin/python3";
        };
      };

      # npx fetches mcpvault on first boot (guest has internet).
      vault = {
        command = "${pkgs.nodejs}/bin/npx";
        args = [ "-y" "@bitbonsai/mcpvault@0.16.0" agentVm.vaultRoot ];
      };
    };

    settings = {
      # Only injected when the module renders config.yaml itself, which configFile above bypasses.
      terminal.cwd = config.services.hermes-agent.workingDirectory;

      providers.ai = aiEndpoint // { model = fleet.ai.model; };

      model = aiEndpoint // {
        provider = "custom:ai";
        default = fleet.ai.model;
        context_length = fleet.ai.contextLength;

        # Detection only probes endpoints it judges local; ours is across the LAN, so declare it.
        supports_vision = true;
      };

      compression.enabled = true;            # auto-summarise old turns

      # Left to `provider: auto` this resolves to a cloud chain (openrouter, then nous),
      # both unauthenticated here; base_url outranks provider and keeps it on the ai host.
      auxiliary.compression = aiEndpoint // { model = fleet.ai.model; };

      # Fired concurrently with the answer and doubled every turn's latency.
      auxiliary.title_generation.enabled = false;

      # Left on "auto" this hides the MCP tools behind a search-then-`tool_call` indirection once the
      # listing passes 5% of context, and the model then calls them without their required arguments.
      tools.tool_search.enabled = "off";

      platform_toolsets.api_server = [ "memory" "session_search" "todo" "vision" ];
      platforms.api_server = {
        enabled = true;
        extra = {
          host = "0.0.0.0";
          port = agentVm.apiPort;

          # Extra dropdown models need an explicit route or they fall back to model.default.
          model_routes = lib.genAttrs [ fleet.ai.codingModel ] (m: aiEndpoint // { provider = "ollama"; model = m; });
        };
      };
    };
  };

  # The vault arrives over virtiofs from compute's CIFS mount, which forces gid 5000 with 0660 and
  # ignores any ownership set here. Group membership is the whole of the agent's write access.
  users.groups.vault.gid = 5000;
  users.users.hermes.extraGroups = [ "vault" ];

  # Inject API_SERVER_KEY at start-up: its virtiofs mount isn't ready at activation (the module's .env merge).
  systemd.services.hermes-agent = {
    serviceConfig = {
      EnvironmentFile = "${agentVm.secretsRoot}/hermes.env";
      # ProtectSystem = "strict" would otherwise keep the vault read-only whatever the group allows.
      ReadWritePaths = [ agentVm.vaultRoot ];
    };
    unitConfig.RequiresMountsFor = [ agentVm.secretsRoot agentVm.vaultRoot ];
  };
}
