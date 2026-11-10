{ lib, pkgs, panel ? "eDP-1", ... }:
pkgs.writeShellApplication {
  name = "displayctl";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.jq
    pkgs.niri
    pkgs.systemd
  ];
  text = ''
    # The laptop panel, as niri names it.
    panel=${lib.escapeShellArg panel}

    ${lib.fileContents ./script.sh}
  '';
  meta.platforms = lib.platforms.linux;
}
