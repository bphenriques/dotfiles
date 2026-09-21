{ lib, pkgs, ... }:
# Manual setup (one-time since it is a trap doing this declaratively):
# 1. Graphics -> set Vulkan, 2x internal resolution, 16x anisotropic filtering.
# 2. Controllers -> configure Emulated Wii Remote for your gamepad.
# 3. Settings -> disable analytics.
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.dolphin-emu ];
}
