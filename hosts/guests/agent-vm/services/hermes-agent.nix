{ lib, pkgs, fleet, agentVm, inputs, ... }:
let
  ollama = {
    provider = "ollama";
    api_key = "ollama";                    # dummy; Ollama needs no auth
    base_url = "http://${fleet.ai.endpoint.host}:${toString fleet.ai.endpoint.port}/v1";
  };
in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  services.hermes-agent = {
    enable = true;
    stateDir = agentVm.stateRoot;
    documents."SOUL.md" = builtins.readFile ../SOUL.md;

    mcpServers = {
      fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
      time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";

      # npx fetches mcpvault on first boot (guest has internet); the vault mount is read-only.
      vault = {
        command = "${pkgs.nodejs}/bin/npx";
        args = [ "-y" "@bitbonsai/mcpvault@0.12.4" agentVm.vaultRoot ];
      };
    };

    settings = {
      model = ollama // {
        default = fleet.ai.model;
        context_length = 65536;              # Hermes requires >=64K; match OLLAMA_CONTEXT_LENGTH
      };
      compression.enabled = true;            # auto-summarise old turns
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
