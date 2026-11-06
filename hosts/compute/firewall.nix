{ config, ... }:
let
  wg = config.selfhost.apps.wireguard.interface;
in
{
  services.openssh.openFirewall = false; # Firewall is managed manually to ensure SSH is restricted (e.g., podman cant access)
  networking.firewall.interfaces = {
    bond0.allowedTCPPorts = [ 22 ];
    ${wg}.allowedTCPPorts = [ 22 ];   # the emergency path if the LAN side is wedged
  };

  # Forward only what is listed; the wireguard-access table narrows wg to full-access peers and WoL.
  networking.firewall.filterForward = true;
  networking.firewall.extraForwardRules = ''
    iifname "${wg}" accept
    iifname "podman0" accept
  '';
}
