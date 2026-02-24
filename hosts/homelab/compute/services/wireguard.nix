{ config, lib, self, ... }:
let
  clientNames = lib.concatLists
    (lib.mapAttrsToList
      (_: u: (lib.map (d: "${u.username}-${d}") u.services.wireguard.devices))
      config.custom.home-server.enabledUsers.wireguard);
in
{
  custom.home-server.wireguard = {
    enable = true;
    address = "10.100.0.1/24";
    allowedIPs = "10.100.0.0/24";
    dns = "1.1.1.1";
    endpoint = self.settings.services.wireguard.endpoint;
    smtpFrom = self.settings.smtp.from;
    clients = clientNames;
  };
}