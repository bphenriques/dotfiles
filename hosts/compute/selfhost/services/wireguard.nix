{ config, private, ... }:
{
  selfhost.vpn.wireguard = {
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
}
