# Configures the auth VM as a microvm running on compute.
# The VM is isolated but can be reached via internal bridge network.
#
# Note: The auth VM's NixOS configuration is defined in nixosConfigurations.auth
# This module just declares that it should run as a microvm on this host.

{ config, lib, pkgs, self, ... }:
let
  networking = import ../networking.nix;
  bridgeName = networking.bridge.name;
in
{
  # Network bridge for VM communication
  # See: https://microvm-nix.github.io/microvm.nix/simple-network.html
  networking.useNetworkd = true;
  systemd.network = {
    # Create the bridge device
    netdevs."10-${bridgeName}".netdevConfig = {
      Kind = "bridge";
      Name = bridgeName;
    };

    # Configure the bridge with host IP
    networks."10-${bridgeName}" = {
      matchConfig.Name = bridgeName;
      networkConfig.ConfigureWithoutCarrier = true;
      addresses = [{ Address = "${networking.bridge.gateway}/24"; }];
    };

    # Attach VM TAP interfaces to the bridge
    networks."11-microvm" = {
      matchConfig.Name = "vm-*";
      networkConfig.Bridge = bridgeName;
    };
  };

  # NAT for VM internet access
  networking.nat = {
    enable = true;
    internalInterfaces = [ bridgeName ];
    externalInterface = "wlp4s0";  # Laptop WiFi interface. Replace with the right interface in compute.
  };

  # Resolve auth domain to internal IP for host<->VM API calls
  networking.hosts.${networking.vm.auth.ip} = [ "auth.${self.settings.compute.domain}" ];

  microvm.vms.auth = {
    flake = self;
    restartIfChanged = true;
  };
}
