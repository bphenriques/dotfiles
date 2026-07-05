{ config, private, ... }:
{
  selfhost.apps.wireguard = {
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
    };
  };

  # Keep the WireGuard endpoint's record pointed at the current (dynamic) public IP.
  sops.secrets."desec/token" = { };
  selfhost.apps.desec = {
    enable = true;
    tokenFile = config.sops.secrets."desec/token".path;
    domains = [ config.selfhost.apps.wireguard.endpoint ];
  };
}
