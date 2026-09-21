{ fleet, cvVm, ... }:
let
  adminUser = "bphenriques";
in
{
  imports = [
    ./settings.nix
    ../../../profiles/nixos/base.nix
    ./microvm.nix
    ./services
  ];

  custom.microvm.guest = {
    stateRoot = cvVm.dataRoot;                    # SSH host key / sops age identity live here
    ingressPorts = [ cvVm.traefikMetricsPort ];   # Traefik metrics, scraped by compute over the bridge
  };

  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "26.05";
}
