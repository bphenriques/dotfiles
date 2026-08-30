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
in
{
  imports = [ inputs.omp.homeManagerModules.default ];

  home.sessionVariables.OLLAMA_BASE_URL =
    let endpoint = osConfig.custom.fleet.ai.endpoint;
    in "http://${endpoint.host}:${toString endpoint.port}";
  home.file.".omp/agent/themes/onedark-transparent.json".source = jsonFormat.generate "onedark-transparent.json" onedarkTransparent;

  programs.omp = {
    enable = true;

    settings = {
      tools.approvalMode = "write";
      secrets.enabled = true;       # Redact credentials before they reach the provider.

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
