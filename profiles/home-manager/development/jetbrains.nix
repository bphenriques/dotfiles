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
          match app-id="jetbrains-idea"
          open-maximized-to-edges true  // Reserve fullscreen for immersive tasks
        }
      ''
    ];
  };

  programs.git.ignores = [
    ".idea"
    ".iml"
  ];
}
