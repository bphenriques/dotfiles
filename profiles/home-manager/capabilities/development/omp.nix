{ inputs, lib, pkgs, osConfig, ... }:
let
  onedarkTransparent = lib.recursiveUpdate
    (lib.importJSON "${inputs.omp}/packages/coding-agent/src/modes/theme/defaults/dark-one.json")
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

  inherit (osConfig.custom.fleet) ai;
in
{
  imports = [ inputs.omp.homeManagerModules.default ];

  # Only reaches shells started after the next login; omp defaults to localhost until then.
  home.sessionVariables = {
    OLLAMA_BASE_URL = "http://${ai.endpoint.host}:${toString ai.endpoint.port}";
    OLLAMA_CONTEXT_LENGTH = toString ai.contextLength;   # `/api/show` reports the training window
  };
  home.file.".omp/agent/themes/onedark-transparent.json".source = jsonFormat.generate "onedark-transparent.json" onedarkTransparent;

  programs.omp = {
    enable = true;

    settings = {
      tools.approvalMode = "write";
      secrets.enabled = true;       # Redact credentials before they reach the provider.

      # `/model` still switches freely until the next home-manager switch restores these.
      # `:high` is the thinking selector; Ollama's /v1 drops reasoning_effort, so it only bites elsewhere.
      modelRoles.default = "ollama/${ai.codingModel}:high";
      # Pinned so background tasks stay local, on the MoE because it decodes 3x the dense coder.
      modelRoles.tiny = "ollama/${ai.model}";

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
}
