{ pkgs, config, lib, self, ... }:
with lib;
let
  cfg = config.custom.proton-run;
  proton-run = pkgs.writeShellApplication {
    name = "proton-run";
    text = ''
      mkdir -p ${cfg.defaultProtonDir}
      if [ "$#" -gt 0 ]; then
        STEAM_COMPAT_DATA_PATH="${cfg.defaultProtonDir}" \
          STEAM_COMPAT_CLIENT_INSTALL_PATH="${cfg.defaultProtonDir}" \
          ${pkgs.steam-run}/bin/steam-run ${self.pkgs.proton-ge-custom}/bin/proton run "$@"
      else
        echo "No argument provided"
      fi
    '';
  };

  mimeTypes = [
    "application/x-ms-dos-executable"
    "application/x-msi"
    "application/x-ms-shortcut"
    "application/vnd.microsoft.portable-executable"
  ];

  proton-run-desktop-item =
    (pkgs.makeDesktopItem {
      exec = "proton-run %f";
      name = "Proton Launcher";
      type = "Application";
      desktopName = "Proton Launcher";
      categories = [ "Utility" "Game" ];
      icon = "wine";
      mimeTypes = mimeTypes;
    });

  setDefault = types: target: foldl' (acc: type: acc // { "${type}" = target; }) { } types;
in {
  options.custom.proton-run = with types; {
    enable = mkEnableOption "proton-run";
    defaultProtonDir = mkOption {
      type = str;
      description = mdDoc ''Default location of catch-all prefix'';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      protonup-qt       # Manage Proton versions
      protontricks      # Install utility within proton
      proton-run        # Run .exe from terminal
      proton-run-desktop-item
    ];

    xdg.mime.defaultApplications = setDefault mimeTypes "Proton Launcher.desktop";
  };
}
