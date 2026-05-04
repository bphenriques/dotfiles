{ lib, pkgs, writeNushellScript, ... }:
let
  script = writeNushellScript "fin3" ./fin3.nu;
in
pkgs.writeShellApplication {
  name = "fin3";
  runtimeInputs = [
    pkgs.nushell
    pkgs.hledger
    pkgs.hledger-ui
    pkgs.hledger-web
    pkgs.youplot
    pkgs.inotify-tools
    # zellij is optional — fin3 edit falls back to plain $EDITOR when absent.
    # When installed via programs.zellij (see profiles/home-manager/development/zellij.nix), it is already on PATH.
  ];
  text = ''
    exec nu --no-config-file ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
