{ shareVm, ... }:
{
  imports = [
    ./settings.nix
    ../../../profiles/nixos/guest.nix
    ./microvm.nix
    ./services
  ];

  custom.microvm.guest = {
    stateRoot = shareVm.dataRoot;                   # SSH host key / sops age identity live here
    ingressPorts = [ shareVm.traefikMetricsPort ];  # Traefik metrics, scraped by compute over the bridge
  };

  system.stateVersion = "26.05";
}
