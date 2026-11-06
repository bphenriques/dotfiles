{ lib, pkgs, ... }:
# Manual one-time setup: Settings, Graphics, Vulkan backend at 3x.
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.ppsspp ];
}
