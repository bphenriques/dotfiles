{ lib, pkgs, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = with pkgs; [ obsidian ];

  custom.programs.niri.windowRules.byApp = [
    ''
      window-rule {
        match app-id="obsidian"
        open-maximized-to-edges true
      }
    ''
  ];
}