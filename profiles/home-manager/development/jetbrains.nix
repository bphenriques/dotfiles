{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ (pkgs.jetbrains.idea-oss.override { forceWayland = true; }) ];

  custom.programs.niri.windowRules.byApp = [
    ''
      window-rule {
        match app-id="jetbrains-idea"
        open-maximized-to-edges true  // Reserve fullscreen for immersive tasks
      }
    ''
  ];

  programs.git.ignores = [
    ".idea"
    ".iml"
  ];
}
