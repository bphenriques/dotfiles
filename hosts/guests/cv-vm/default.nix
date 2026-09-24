{ cvVm, ... }:
{
  imports = [
    ./settings.nix
    ../../../profiles/nixos/guest.nix
    ./microvm.nix
    ./services
  ];

  custom.microvm.guest = {
    stateRoot = cvVm.dataRoot;                    # SSH host key / sops age identity live here
    ingressPorts = [ cvVm.traefikMetricsPort ];   # Traefik metrics, scraped by compute over the bridge
  };

  system.stateVersion = "26.05";
}
