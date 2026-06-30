{ self, inputs, config, ... }:
let
  inherit (config.custom.fleet.computeMicrovm) bridge;
in
{
  imports = [
    inputs.microvm.nixosModules.host
    ./share-vm.nix
  ];

  # Narrow override of the no-forwarding baseline: reach guests via `ssh -J compute` (admin).
  services.openssh.extraConfig = ''
    Match User ${config.users.users.bphenriques.name}
      AllowTcpForwarding yes
  '';

  # Networking: translate external requests from the bridge to the bond0 network interface
  networking = {
    nat = {
      enable = true;
      internalInterfaces = [ bridge.name ];
      externalInterface = "bond0";
    };
    bridges.${bridge.name}.interfaces = [ ];        # Empty b/c VM TAP interfaces are attached dynamically at runtime.
    interfaces.${bridge.name}.ipv4.addresses = [{   # Assigns host's IP on this network, acting as the default gateway for the VMs.
      address = bridge.gateway;
      inherit (bridge) prefixLength;
    }];
  };

  # Guest Isolation: gets no input to compute's services nor is allowed to access internal network
  networking.firewall.extraForwardRules = ''
    iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "Internet only. Drop private IP packets (RFC 1918)"
  '';
}
