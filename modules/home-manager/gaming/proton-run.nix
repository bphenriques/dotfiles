{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.custom.proton-run;
  # Script to run proton on a static prefix (because doesn't really matter for ad-hoc runs)
  # TODO: Avoid using steam-run. Alternative is to use buildFHSEnv by-hand (if I understood correctly...)
  proton-run = pkgs.writeShellApplication {
    name = "proton-run";
    runtimeInputs = with pkgs; [ nur.repos.ataraxiasjel.proton-ge steam-run ];
    text = ''
      if [ "$#" -gt 0 ]; then
        STEAM_COMPAT_DATA_PATH="${cfg.defaultProtonDir}" \
          STEAM_COMPAT_CLIENT_INSTALL_PATH="${cfg.defaultProtonDir}" \
          ${pkgs.steam-run}/bin/steam-run ${pkgs.nur.repos.ataraxiasjel.proton-ge}/bin/proton run "$@"
      fi
    '';
  };

  proton-run-desktop-item =
    (pkgs.makeDesktopItem {
      exec = "proton-run %f";
      name = "Proton Launcher";
      type = "Application";
      desktopName = "Proton Launcher";
      categories = [ "Utility" "Game" ];
      icon = "wine";
      mimeTypes = ["application/x-ms-dos-executable" "application/x-msi" "application/x-ms-shortcut"];
    });
in {
  options.custom.proton-run = with types; {
    enable = mkEnableOption ''proton-run'';
    defaultProtonDir = mkOption {
      type = str;
      default = null;
      description = mdDoc ''Default location of ad-hoc proton'';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      protonup-qt       # Manage Proton versions
      protontricks      # Install utility within proton
      proton-run        # Run .exe from termional
      proton-run-desktop-item
    ];
  };
}
