{ lib, pkgs, writeNushellScript, ... }:
let
  dashboard = writeNushellScript "fin-dashboard" ./dashboard.nu;
in
pkgs.writeShellApplication {
  name = "fin";
  runtimeInputs = [ pkgs.hledger pkgs.hledger-ui pkgs.hledger-web pkgs.nushell pkgs.youplot ];
  text = ''
    FIN_DASHBOARD="${dashboard}"
  '' + lib.fileContents ./script.sh;
  meta.platforms = lib.platforms.linux;
}
