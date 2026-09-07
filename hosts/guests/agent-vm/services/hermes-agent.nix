{ lib, pkgs, fleet, agentVm, inputs, ... }:
let
  endpoint = "http://${fleet.ai.endpoint.host}:${toString fleet.ai.endpoint.port}/v1";
  ollama = {
    provider = "ollama";
    api_key = "ollama";                    # dummy; Ollama needs no auth
    base_url = endpoint;
  };

in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  services.hermes-agent = {
    enable = true;
    stateDir = agentVm.stateRoot;
    hermesHomeFiles."SOUL.md" = builtins.readFile ../SOUL.md;
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
      };

      # npx fetches mcpvault on first boot (guest has internet).
      vault = {
        command = "${pkgs.nodejs}/bin/npx";
        args = [ "-y" "@bitbonsai/mcpvault@0.12.4" agentVm.vaultRoot ];

        # The vault mounts read-only, so mcpvault's write half can only ever fail; hide it from the model.
        tools.exclude = [
          "write_note" "patch_note" "delete_note" "move_note" "move_file" "update_frontmatter" "manage_tags"
        ];
      };
    };

    settings = {
      providers.ai = {
        inherit (ollama) api_key base_url;
        model = fleet.ai.model;
      };

      model = ollama // {
        provider = "custom:ai";
        default = fleet.ai.model;
        context_length = 65536;              # Hermes requires >=64K; match OLLAMA_CONTEXT_LENGTH
      };

      compression.enabled = true;            # auto-summarise old turns

      # Left to `provider: auto` this resolves to a cloud chain (openrouter, then nous),
      # both unauthenticated here; base_url outranks provider and keeps it on the ai host.
      auxiliary.compression = {
        inherit (ollama) api_key base_url;
        model = fleet.ai.model;
      };

      platform_toolsets.api_server = [ "memory" "session_search" "todo" ];
      platforms.api_server = {
        enabled = true;
        extra = {
          host = "0.0.0.0";
          port = agentVm.apiPort;

          # Extra dropdown models need an explicit route or they fall back to model.default.
          model_routes = lib.genAttrs fleet.ai.extraModels (m: ollama // { model = m; });
        };
      };
    };
  };

  # Inject API_SERVER_KEY at start-up: its virtiofs mount isn't ready at activation (the module's .env merge).
  systemd.services.hermes-agent = {
    serviceConfig.EnvironmentFile = "${agentVm.secretsRoot}/hermes.env";
    unitConfig.RequiresMountsFor = [ agentVm.secretsRoot agentVm.vaultRoot ];
  };
}
