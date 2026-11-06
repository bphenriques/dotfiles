{ config, lib, pkgs, alfredo, alfredoVm, private, inputs, ... }:
let
  # ai is this guest's host, reached through the bridge gateway; the seal drops its LAN address.
  endpoint = "http://${config.my.microvm.guest.gateway}:${toString config.fleet.ai.endpoint.port}/v1";
  aiEndpoint = {
    api_key = "ollama";   # dummy; Ollama needs no auth
    base_url = endpoint;
  };
in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  # Installed verbatim: the module deep-merges `settings` and never drops a key removed from Nix.
  services.hermes-agent.configFile =
    pkgs.writeText "hermes-config.yaml" (builtins.toJSON config.services.hermes-agent.settings);

  services.hermes-agent = {
    enable = true;
    stateDir = alfredoVm.stateRoot;
    hermesHomeFiles."SOUL.md" = alfredo.soul + "\n" + builtins.readFile private.household;
    hermesHomeFiles.".no-bundled-skills" = "";   # seed only `hermes-agent`; the rest come from alfredo's skills

    # Gateway sessions load AGENTS.md from terminal.cwd.
    workingDirectory = "${alfredoVm.stateRoot}/workspace";
    documents."AGENTS.md" = alfredo.workspace;
    addToSystemPackages = true;   # `hermes` CLI in the guest, for debugging against the live state

    extraPackages = alfredo.packages;
    inherit (alfredo) mcpServers;

    settings = lib.mkMerge [
      alfredo.settings
      {
        # Only injected when the module renders config.yaml itself, which configFile above bypasses.
        terminal.cwd = config.services.hermes-agent.workingDirectory;

        providers.ai = aiEndpoint // { model = config.fleet.ai.model; };

        model = aiEndpoint // {
          provider = "custom:ai";
          default = config.fleet.ai.model;
          context_length = config.fleet.ai.contextLength;

          # Detection only probes endpoints it judges local; ours is across the LAN, so declare it.
          supports_vision = true;
        };

        compression.enabled = true;            # auto-summarise old turns

        # `provider: auto` would resolve to an unauthenticated cloud chain; base_url keeps it on the ai host.
        auxiliary.compression = aiEndpoint // { model = config.fleet.ai.model; };

        # Fired concurrently with the answer and doubled every turn's latency.
        auxiliary.title_generation.enabled = false;

        platforms.api_server = {
          enabled = true;
          extra = {
            host = "0.0.0.0";
            inherit (config.fleet.ai.assistantEndpoint) port;

            # Extra dropdown models need an explicit route or they fall back to model.default.
            model_routes = lib.genAttrs [ config.fleet.ai.codingModel ] (m: aiEndpoint // { provider = "custom:ai"; model = m; });
          };
        };
      }
    ];
  };

  environment.systemPackages = [ alfredo.cookidoo alfredo.meals alfredo.drafts ];   # `sudo -u hermes cookidoo login` once; `meals propose` to check

  # The mounts force their group with 0660, so membership is the whole of the agent's write access.
  users.users.hermes.extraGroups = [ "vault" "recipes" ];

  # A copy of the key compute generates for Open WebUI (hosts/compute/selfhost/services/hermes.nix).
  sops.secrets."hermes/api-key" = { };
  sops.templates."hermes.env".content = "API_SERVER_KEY=${config.sops.placeholder."hermes/api-key"}\n";
  services.hermes-agent.environmentFiles = [ config.sops.templates."hermes.env".path ];

  # ProtectSystem = "strict" would otherwise keep the vault read-only whatever the group allows.
  systemd.services.hermes-agent.serviceConfig.ReadWritePaths = [ alfredoVm.vaultRoot alfredoVm.recipesRoot ];
}
