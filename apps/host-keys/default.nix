{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "host-keys";
  runtimeInputs = [ pkgs.openssh pkgs.ssh-to-age pkgs.nix ];
  text = lib.fileContents ./script.sh;
  meta.description = "Capture a microvm guest's host key for known-hosts pinning + sops";
  meta.platforms = lib.platforms.all;
}
