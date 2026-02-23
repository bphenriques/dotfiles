# Configures microvms running on compute.
# VMs are isolated but can be reached via internal bridge network.
#
# Note: Each VM's NixOS configuration is defined in nixosConfigurations.<name>
# This module just declares that they should run as microvms on this host.

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

  # NAT for VM internet access + port forwarding to gateway VM
  networking.nat = {
    enable = true;
    internalInterfaces = [ bridgeName ];
    externalInterface = "wlp4s0";  # Laptop WiFi interface. Replace with the right interface in compute.

    # Forward external traffic to gateway VM (Pangolin)
    forwardPorts = [
      { destination = "${networking.vm.gateway.ip}:443"; proto = "tcp"; sourcePort = 443; }
      { destination = "${networking.vm.gateway.ip}:443"; proto = "udp"; sourcePort = 443; }
      { destination = "${networking.vm.gateway.ip}:51820"; proto = "udp"; sourcePort = 51820; }
    ];
  };

  # Resolve VM domains to internal IPs for host<->VM API calls
  networking.hosts = {
    ${networking.vm.auth.ip} = [ "auth.${self.settings.compute.domain}" ];
    ${networking.vm.gateway.ip} = [
      "pangolin.${self.settings.compute.domain}"
      "gateway.${self.settings.compute.domain}"
    ];
  };

  microvm.vms = {
    auth = {
      flake = self;
      restartIfChanged = true;
    };
    gateway = {
      flake = self;
      restartIfChanged = true;
    };
  };
}
