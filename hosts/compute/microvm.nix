{ self, config, inputs, ... }:
let
  inherit (config.custom.fleet.microvm) bridge;
in
{
  imports = [ inputs.microvm.nixosModules.host ];

  microvm.autostart = [ "personal-agent" ];

  # Grant me the ability to jump to the microvm from compute (`ssh -J compute bphenriques@<vm>`).
  services.openssh.extraConfig = ''
    Match User ${config.users.users.bphenriques.name}
      AllowTcpForwarding yes
  '';

  # Bridge interface for the VMs
  networking = {
    bridges.${bridge.name}.interfaces = [ ];
    interfaces.${bridge.name}.ipv4.addresses = [{
      inherit (bridge) prefixLength;
      address = bridge.gateway;
    }];

    nat = {
      enable = true;
      internalInterfaces = [ bridge.name ];
      externalInterface = "bond0";
    };

    firewall.trustedInterfaces = [ bridge.name ];
  };

  microvm.vms.personal-agent = {
    flake = self;
    restartIfChanged = true;
  };

  custom.homelab.wireguard.trustedForwardInterfaces = [ bridge.name ]; # Ensure wireguard can forward requests to this interface
  custom.homelab.serviceAccounts.personal-agent = {
    description = "vault-sync identity for personal-agent";
    services.gitea.enable = true;
  };
}
