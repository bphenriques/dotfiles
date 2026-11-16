{ inputs, lib, pkgs, osConfig, ... }:
let
  onedarkTransparent = lib.recursiveUpdate
    (lib.importJSON "${inputs.omp}/packages/tui/src/theme/defaults.json")."dark-one"
    {
      name = "onedark-transparent";
      vars = {
        userMsgBg = "";
        customMsgBg = "";
        toolPendingBg = "";
        toolSuccessBg = "";  # toolErrorBg stays filled so failures still stand out.
      };
    };

  jsonFormat = pkgs.formats.json { };
  yamlFormat = pkgs.formats.yaml { };

  inherit (osConfig.fleet) ai;

  models.providers.ai = {
    baseUrl = "http://${ai.endpoint.host}:${toString ai.endpoint.port}/v1";
    api = "openai-completions";
    auth = "none";
    models = [{
      id = ai.model;
      reasoning = true;
      input = [ "text" "image" ];
      contextWindow = ai.contextLength;
      compat.supportsReasoningEffort = true;
    }];
  };
in
{
  imports = [ inputs.omp.homeManagerModules.default ];

  config = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    home.file.".omp/agent/models.yml".source = yamlFormat.generate "models.yml" models;
    home.file.".omp/agent/themes/onedark-transparent.json".source = jsonFormat.generate "onedark-transparent.json" onedarkTransparent;

    programs.omp = {
      enable = true;

      settings = {
        tools.approvalMode = "write";
        secrets.enabled = true;       # Redact credentials before they reach the provider.

        # `/model` switches until the next switch restores these; `:high` reaches Gufo as reasoning_effort.
        modelRoles.default = "ai/${ai.model}:high";
        modelRoles.tiny = "ai/${ai.model}";   # pinned so background tasks stay local

        # Nix owns the version.
        startup.checkUpdate = false;
        marketplace.autoUpdate = "off";

        startup.quiet = true;
        startup.setupWizard = false;

        # Minimal chrome that lets stylix's 0.92 terminal opacity through.
        theme.dark = "onedark-transparent";
        composer.shape = "borderless";
        statusLine.preset = "compact";
        statusLine.transparent = true;
      };
    };
  };
}
