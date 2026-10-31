{ config, lib, pkgs, fleet, alfred, alfredVm, private, inputs, ... }:
let
  # ai is this guest's host, reached through the bridge gateway; the seal drops its LAN address.
  endpoint = "http://${config.my.microvm.guest.gateway}:${toString fleet.ai.endpoint.port}/v1";
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
            model_routes = lib.genAttrs [ fleet.ai.codingModel ] (m: aiEndpoint // { provider = "custom:ai"; model = m; });
          };
        };
      }
    ];
  };

  environment.systemPackages = [ alfred.cookidoo alfred.meals alfred.drafts ];   # `sudo -u hermes cookidoo login` once; `meals propose` to check

  # The mounts force their group with 0660, so membership is the whole of the agent's write access.
  users.users.hermes.extraGroups = [ "vault" "recipes" ];

  # A copy of the key compute generates for Open WebUI (hosts/compute/selfhost/services/hermes.nix).
  sops.secrets."hermes/api-key" = { };
  sops.templates."hermes.env".content = "API_SERVER_KEY=${config.sops.placeholder."hermes/api-key"}\n";
  services.hermes-agent.environmentFiles = [ config.sops.templates."hermes.env".path ];

  # ProtectSystem = "strict" would otherwise keep the vault read-only whatever the group allows.
  systemd.services.hermes-agent.serviceConfig.ReadWritePaths = [ alfredVm.vaultRoot alfredVm.recipesRoot ];
}
