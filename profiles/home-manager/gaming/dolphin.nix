{ lib, pkgs, ... }:
# Manual one-time setup: Graphics (Vulkan, 2x, 16x anisotropic), Controllers (Wii Remote), Settings (disable analytics).
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.dolphin-emu ];
}
