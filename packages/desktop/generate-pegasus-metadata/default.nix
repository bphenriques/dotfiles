{ lib, pkgs, writeNushellScript, ... }:
let
  script = writeNushellScript "generate-pegasus-metadata" ./script.nu;
in
pkgs.writeShellApplication {
  name = "generate-pegasus-metadata";
  runtimeInputs = [ pkgs.nushell ];
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
