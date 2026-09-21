{ lib, pkgs, ... }:
# Manual setup (one-time since it is a trap doing this declaratively):
# 1. Settings → Graphics → set Vulkan backend, 3x resolution.
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.ppsspp ];
}
