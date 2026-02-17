# Configures the auth VM as a microvm running on compute.
# The VM is isolated but can be reached via internal bridge network.
#
# Note: The auth VM's NixOS configuration is defined in nixosConfigurations.auth
# This module just declares that it should run as a microvm on this host.

{ config, lib, pkgs, self, ... }:
let
  authVmIp = "10.20.0.10";
  hostBridgeIp = "10.20.0.1";
  bridgeName = "br-auth";
in
{
  # Network bridge for VM communication (https://microvm-nix.github.io/microvm.nix/simple-network.html)
  networking = {
    bridges.${bridgeName}.interfaces = [];
    interfaces.${bridgeName}.ipv4.addresses = [{
      address = hostBridgeIp;
      prefixLength = 24;
    }];

    # NAT for VM internet access
    nat = {
      enable = true;
      internalInterfaces = [ bridgeName ];
      externalInterface = "eth0";
    };
  };

  microvm.vms.auth = {
    flake = self;
    updateFlake = "git+file:///home/bphenriques/.dotfiles";
    restartIfChanged = true;
  };
}
