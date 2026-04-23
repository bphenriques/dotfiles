{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.jetbrains.idea-oss ];

  custom.programs.niri.windowRules = {
    byType.popups = [
      ''app-id="jetbrains-idea" title=r#"^$"#''
    ];

    byApp = [
      ''
        window-rule {
          match app-id="jetbrains-idea" at-startup=true  // Placement only on login
          open-on-workspace "2"
        }
      ''
      ''
        window-rule {
          match app-id="jetbrains-idea"
          open-maximized-to-edges true  // Reserve fullscreen for immersive tasks
        }
      ''
    ];
  };
}
