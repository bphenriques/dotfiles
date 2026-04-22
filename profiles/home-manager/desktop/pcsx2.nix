{ lib, pkgs, osConfig, ... }:
# PCSX2 treats config files as mutable runtime state. Do not manage them declaratively.
# Manual setup (one-time): run PCSX2 → complete the setup wizard → set BIOS path to
#   ${osConfig.custom.homelab.paths.media.gaming.emulation.bios}
# Manual setup (one-time): Settings → Graphics → set 3x internal resolution.
# Manual setup (one-time): Settings → Controllers → configure gamepad for Port 1.
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.pcsx2 ];

  custom.programs.niri.windowRules = {
    byApp = [
      ''
        window-rule {
          match app-id="PCSX2"
          open-fullscreen true
          open-focused true
        }
      ''
    ];
    overrides = [
      ''
        window-rule {
          match app-id="PCSX2"
          opacity 1.0
        }
      ''
    ];
  };
}
