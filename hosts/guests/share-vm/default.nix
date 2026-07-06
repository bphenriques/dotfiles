{ fleet, shareVm, ... }:
let
  adminUser = "bphenriques";
in
{
  imports = [
    ./settings.nix
    ../../../profiles/nixos/microvm-guest.nix
    ./microvm.nix
    ./services
  ];

  homelab.microvm.guest = {
    enable = true;
    stateRoot = shareVm.dataRoot;                 # SSH host key / sops age identity live here
    ingressPorts = [ shareVm.traefikMetricsPort ]; # Traefik metrics, scraped by compute over the bridge
  };

  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "26.05";
}
