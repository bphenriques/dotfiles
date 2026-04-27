{ lib, pkgs, builders, fleetHostIPs, ... }:
let
  hostIPsJSON = pkgs.writeText "fleet-host-ips.json" (builtins.toJSON fleetHostIPs);
in
builders.mkFishShellPlugin {
  drv = pkgs.writeShellApplication {
    name = "dotfiles";
    runtimeInputs = [ pkgs.nvd pkgs.nix-output-monitor pkgs.jq ];
    text = ''
      FLEET_HOST_IPS="${hostIPsJSON}"
    '' + lib.fileContents ./script.sh;
    meta.platforms = lib.platforms.all;
  };
  fishPluginSrc = ./fish-plugin;
}
