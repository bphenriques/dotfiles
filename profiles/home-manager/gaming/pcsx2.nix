{ lib, pkgs, ... }:
# Manual one-time setup: wizard BIOS path to <media share>/gaming/emulation/bios, 3x resolution, gamepad on Port 1.
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.pcsx2 ];
}
