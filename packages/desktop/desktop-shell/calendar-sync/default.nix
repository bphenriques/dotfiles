{ lib, pkgs, writeNushellScript, ... }:
let
  script = writeNushellScript "desktop-shell-calendar-sync" ./script.nu;
in
pkgs.writeShellApplication {
  name = "desktop-shell-calendar-sync";
  runtimeInputs = [ pkgs.nushell pkgs.vdirsyncer pkgs.khal pkgs.coreutils ];
  text = ''
    exec nu ${script} "$@"
  '';
  # Where the list starts from: its format, with an example commented out.
  passthru.sample = ./calendars.yaml;
  meta.platforms = lib.platforms.linux;
}
