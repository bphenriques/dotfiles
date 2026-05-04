{ lib, pkgs, ... }:
let
  src = pkgs.runCommand "fin-checked" { } ''
    mkdir -p $out/finlib
    cp ${./fin.nu} $out/fin.nu
    cp ${./finlib/core.nu} $out/finlib/core.nu
    cp ${./finlib/budget.nu} $out/finlib/budget.nu
    cp ${./finlib/ledger.nu} $out/finlib/ledger.nu
    cp ${./finlib/reports.nu} $out/finlib/reports.nu
    cp ${./finlib/render.nu} $out/finlib/render.nu
    cd $out && ${lib.getExe pkgs.nushell} --no-config-file fin.nu --help > /dev/null
  '';
in
pkgs.writeShellApplication {
  name = "fin";
  runtimeInputs = [
    pkgs.nushell
    pkgs.hledger
    pkgs.hledger-ui
    pkgs.hledger-web
    pkgs.youplot
    pkgs.inotify-tools
    # zellij is optional — fin edit falls back to plain $EDITOR when absent.
    # When installed via programs.zellij (see profiles/home-manager/development/zellij.nix), it is already on PATH.
  ];
  text = ''
    exec nu --no-config-file ${src}/fin.nu "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
