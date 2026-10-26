{ config, lib, pkgs, fleet, alfred, alfredVm, private, inputs, ... }:
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
    stateDir = alfredVm.stateRoot;
    hermesHomeFiles."SOUL.md" = alfred.soul + "\n" + builtins.readFile private.household;
    hermesHomeFiles.".no-bundled-skills" = "";   # seed only `hermes-agent`; the rest come from alfred's skills

    # Gateway sessions load AGENTS.md from terminal.cwd.
    workingDirectory = "${alfredVm.stateRoot}/workspace";
    documents."AGENTS.md" = alfred.environment;
    addToSystemPackages = true;   # `hermes` CLI in the guest, for debugging against the live state

    extraPackages = alfred.packages;
    inherit (alfred) mcpServers;

    settings = lib.mkMerge [
      alfred.settings
      {
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

        platforms.api_server = {
          enabled = true;
          extra = {
            host = "0.0.0.0";
            port = alfredVm.apiPort;

            # Extra dropdown models need an explicit route or they fall back to model.default.
            model_routes = lib.genAttrs [ fleet.ai.codingModel ] (m: aiEndpoint // { provider = "ollama"; model = m; });
          };
        };
      }
    ];
  };

  environment.systemPackages = [ alfred.cookidoo alfred.meals ];   # `sudo -u hermes cookidoo login` once; `meals propose` to check

  # The vault arrives over virtiofs from compute's CIFS mount, which forces gid 5000 with 0660 and
  # ignores any ownership set here. Group membership is the whole of the agent's write access.
  users.groups.vault.gid = 5000;
  users.groups.recipes.gid = 5001;   # the media share's forced gid, for the recipes subtree
  users.users.hermes.extraGroups = [ "vault" "recipes" ];

  # Inject API_SERVER_KEY at start-up: its virtiofs mount isn't ready at activation (the module's .env merge).
  systemd.services.hermes-agent = {
    serviceConfig = {
      EnvironmentFile = "${alfredVm.secretsRoot}/hermes.env";
      # ProtectSystem = "strict" would otherwise keep the vault read-only whatever the group allows.
      ReadWritePaths = [ alfredVm.vaultRoot alfredVm.recipesRoot ];
    };
    unitConfig.RequiresMountsFor = [ alfredVm.secretsRoot alfredVm.vaultRoot alfredVm.recipesRoot ];
  };
}
