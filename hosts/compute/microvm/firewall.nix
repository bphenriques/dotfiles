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
    iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "Internet only. Drop private IP packets (RFC 1918)"
  '';
}
