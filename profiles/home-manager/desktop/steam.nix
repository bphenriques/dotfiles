{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  custom.programs.niri.windowRules = {
    byType.popups = [
      ''app-id="Steam" title=r#"^Steam .+"#''
    ];

    byApp = [
      ''
        window-rule {
          match app-id=r#"^steam_app"#
          open-fullscreen true
          open-focused true
        }
      ''
      ''
        window-rule {
          match app-id="Steam"
          open-maximized true
          scroll-factor 0.5
        }
      ''
    ];

    overrides = [
      ''
        window-rule {
          match app-id=r#"^steam_app"#
          match app-id="Steam"
          opacity 1.0
        }
      ''
    ];
  };
}
