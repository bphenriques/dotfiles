{ lib, pkgs, writeNushellScript, ... }:
let
  script = writeNushellScript "fin2" ./fin2.nu;
in
pkgs.writeShellApplication {
  name = "fin2";
  runtimeInputs = [ pkgs.nushell pkgs.youplot ];
  text = ''
    exec nu --no-config-file ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
