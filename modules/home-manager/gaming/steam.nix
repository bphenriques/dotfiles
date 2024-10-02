{ pkgs, config, lib, ... }:

with lib;
let
  cfg = config.custom.steam;
  steam-desktop-item = (pkgs.makeDesktopItem {
    name = "steam";
    desktopName = "Steam";
    icon = "steam";
    exec = "${pkgs.steam}/bin/steam";
    terminal = false;
    mimeTypes = [ "x-scheme-handler/steam" ];
    categories = [ "Network" "FileTransfer" "Game" ];
  });
in {
  options.custom.steam = with types; {
    enable = mkEnableOption "steam";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      steam-desktop-item
    ];
  };
}
