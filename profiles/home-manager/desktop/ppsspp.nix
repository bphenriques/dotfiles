{ lib, pkgs, ... }:
# PPSSPP treats config files as mutable runtime state. Do not manage them declaratively.
# Manual setup (one-time): open PPSSPP → Settings → Graphics → set Vulkan backend, 3x resolution.
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.ppsspp ];

  custom.programs.niri.windowRules = {
    byApp = [
      ''
        window-rule {
          match app-id="ppsspp"
          open-on-workspace "3"
          open-fullscreen true
          open-focused true
        }
      ''
    ];
    overrides = [
      ''
        window-rule {
          match app-id="ppsspp"
          opacity 1.0
        }
      ''
    ];
  };
}
