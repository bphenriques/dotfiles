{ lib, pkgs, ... }:
# Dolphin treats config files as mutable runtime state. Do not manage them declaratively.
# Manual setup (one-time): open Dolphin -> Graphics -> set Vulkan, 2x internal resolution, 16x anisotropic filtering.
# Manual setup (one-time): open Dolphin -> Controllers -> configure Emulated Wii Remote for your gamepad.
# Manual setup (one-time): open Dolphin -> Settings -> disable analytics.
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.dolphin-emu ];
}
