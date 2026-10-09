{ lib, pkgs, writeNushellScript, ... }:
let
  script = writeNushellScript "handheld-sync" ./script.nu;
in
pkgs.writeShellApplication {
  name = "handheld-sync";
  runtimeInputs = [ pkgs.nushell pkgs.rsync pkgs.openssh pkgs.skyscraper ]; # skyscraper: the miyoo post-sync hook
  text = ''
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
