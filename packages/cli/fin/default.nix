{ lib, pkgs, nushell, inotify-tools, ... }:
let
  src = pkgs.runCommand "fin-checked" { } ''
    mkdir -p $out/finlib
    cp ${./fin.nu} $out/fin.nu
    cp ${./finlib/core.nu} $out/finlib/core.nu
    cp ${./finlib/budget.nu} $out/finlib/budget.nu
    cp ${./finlib/reports.nu} $out/finlib/reports.nu
    cp ${./finlib/render.nu} $out/finlib/render.nu
    cp ${./finlib/markdown.nu} $out/finlib/markdown.nu
    cd $out && ${lib.getExe nushell} --no-config-file fin.nu --help > /dev/null
  '';
in
pkgs.writeShellApplication {
  name = "fin";
  runtimeInputs = [
    nushell
    inotify-tools
  ];
  text = ''
    exec nu --no-config-file ${src}/fin.nu "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
