{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  # home.packages = [ pkgs.flycast ]; Disabled as it does not build.
}
