_:
let
  cvVm = import ./settings.nix;
in
{
  imports = [
    ../../../profiles/nixos/guest.nix
    ./microvm.nix
    ./services
  ];

  _module.args.cvVm = cvVm;

  my.microvm.guest = {
    stateRoot = cvVm.dataRoot;                    # SSH host key / sops age identity live here
    ingressPorts = [ cvVm.traefikMetricsPort ];   # Traefik metrics, scraped by compute over the bridge
  };

  system.stateVersion = "26.05";
}
