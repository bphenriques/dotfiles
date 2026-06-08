# WireGuard opt-in. Access control via the framework's server-side nftables (lanAccess), not
# client AllowedIPs. The framework owns the interface/keys/registry/onboarding/routing.
{ config, private, ... }:
{
  custom.homelab.wireguard = {
    enable = true;
    address = "10.100.0.1/24";
    clientSubnet = "10.100.0.0/24";
    endpoint = private.settings.services.wireguard.endpoint;
    dns = config.custom.fleet.dns;
    name = "bphenr";
    lanAccess = {
      enable = true;
      subnet = config.custom.fleet.lan.subnet;
      masquerade = true;
      # Podman: default bridge 10.88.0.0/16 + user networks 10.89.0.0/16; /15 covers both.
      extraAllowedSubnets = [ "10.88.0.0/15" ];
    };
  };
}
