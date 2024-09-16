{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.custom.proton-run;

  # FIMXE: Also enable gamemode
  proton-run = pkgs.writeShellApplication {
    name = "proton-run";
    text = ''
      mkdir -p ${cfg.defaultProtonDir}
      if [ "$#" -gt 0 ]; then
        STEAM_COMPAT_DATA_PATH="${cfg.defaultProtonDir}" \
            STEAM_COMPAT_CLIENT_INSTALL_PATH="${cfg.defaultProtonDir}" \
          ${pkgs.steam-run}/bin/steam-run ${pkgs.proton-ge-bin}/bin/proton run "$@"
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
      description = mdDoc ''Default location of catch-all prefix'';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      protonup-qt       # Manage Proton versions
      protontricks      # Install utility within proton
      proton-run        # Run .exe from terminal
      proton-run-desktop-item
    ];
  };
}
