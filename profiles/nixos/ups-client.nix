# Watches the fleet UPS and shuts down with it. Server side: hosts/storage/services/ups.nix.
{ config, ... }:
let
  inherit (config.fleet) ups;
in
{
  power.ups = {
    enable = true;
    mode = "netclient";
    upsmon.monitor.${ups.name} = {
      system = "${ups.name}@${ups.host}"; # upsmon MONITOR takes <upsname>@<host>
      powerValue = 1;
      user = config.networking.hostName; # upsd accounts are named after the host
      passwordFile = config.sops.secrets."upsmon/password".path;
      type = "secondary";
    };
  };

  sops.secrets."upsmon/password" = { };
}
