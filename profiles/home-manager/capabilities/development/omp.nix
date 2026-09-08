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

  home.sessionVariables = {
    OLLAMA_BASE_URL = "http://${ai.endpoint.host}:${toString ai.endpoint.port}";

    # Tells omp the real window; it does not change Ollama's runtime num_ctx.
    OLLAMA_CONTEXT_LENGTH = toString ai.contextLength;
  };
  home.file.".omp/agent/themes/onedark-transparent.json".source = jsonFormat.generate "onedark-transparent.json" onedarkTransparent;

  programs.omp = {
    enable = true;

    settings = {
      tools.approvalMode = "write";
      secrets.enabled = true;       # Redact credentials before they reach the provider.

      # `/model` still switches freely until the next home-manager switch restores these.
      # `tiny` is pinned too: left unset, background tasks fall back to a cloud model.
      modelRoles.default = "ollama/${ai.codingModel}";
      modelRoles.tiny = "ollama/${ai.codingModel}";

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
