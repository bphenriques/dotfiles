{ lib, pkgs, ... }:
{
  home.packages = lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.jetbrains.idea ];  # macOS: Homebrew cask

  my.programs.niri.windowRules.byApp = lib.mkIf pkgs.stdenv.hostPlatform.isLinux [
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
