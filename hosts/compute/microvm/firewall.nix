# Egress: NAT lets the VM reach the internet (tailscale/ACME); the forward DROP seals it
# off the LAN, so a compromised app (no hypervisor escape needed) can't pivot to the NAS or
# the fleet. The VM never needs the LAN — curation is laptop→VM and compute always initiates.
{ config, ... }:
let
  inherit (config.custom.fleet.computeMicrovm) bridge;
in
{
  networking.nat = {
    enable = true;
    internalInterfaces = [ bridge.name ];
    externalInterface = "bond0";
  };
  networking.firewall.extraForwardRules = ''
    iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "sealed VM: internet only, no LAN pivot"
  '';
}
