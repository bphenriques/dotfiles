{ config, private, ... }:
{
  selfhost.apps.wireguard = {
    enable = true;
    address = "10.100.0.1/24";
    clientSubnet = "10.100.0.0/24";
    fullAccessSubnet = "10.100.0.0/28";   # Only my own devices; everything above reaches Traefik alone
    endpoint = private.settings.services.wireguard.endpoint;
    dns = config.fleet.dns;
    lanAccess = {
      enable = true;
      subnet = config.fleet.lan.subnet;
      serverAddress = config.fleet.lan.hosts.compute;
      masquerade = true;
      wakeOnLan = true;   # Wake a powered-off fleet host from the VPN
    };
  };

  # Ensure WireGuard endpoint is up to date as my home IP may change
  sops.secrets."desec/token" = { };
  selfhost.apps.desec = {
    enable = true;
    tokenFile = config.sops.secrets."desec/token".path;
    domains = [ config.selfhost.apps.wireguard.endpoint ];
  };
}
