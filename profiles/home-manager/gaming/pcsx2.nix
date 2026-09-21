{ lib, pkgs, ... }:
# Manual setup (one-time since it is a trap doing this declaratively):
# 1. Complete the setup wizard → set BIOS path to ${osConfig.custom.shares.media.root}/gaming/emulation/bios
# 2. Graphics → set 3x internal resolution.
# 3. Controllers → configure gamepad for Port 1.
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.pcsx2 ];
}
