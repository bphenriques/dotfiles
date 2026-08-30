{ config, ... }:
{
  sops.secrets = {
    "ups/storage-password" = { };
    "ups/compute-password" = { };
  };

  power.ups = {
    enable = true;
    mode = "netserver";
    openFirewall = false; # ../firewall.nix opens 3493 to compute only.
    ups.storage = {
      driver = "usbhid-ups";
      port = "auto";
      description = "Storage UPS";
    };
    # DHCP assigns the LAN address, so upsd cannot bind a literal one at start.
    upsd.listen = [ { address = "0.0.0.0"; } ];
    users = {
      storage = {
        passwordFile = config.sops.secrets."ups/storage-password".path;
        upsmon = "primary";
      };
      compute = {
        passwordFile = config.sops.secrets."ups/compute-password".path;
        upsmon = "secondary";
      };
    };
    upsmon.monitor.storage = {
      system = "storage@localhost";
      powerValue = 1;
      user = "storage";
      passwordFile = config.sops.secrets."ups/storage-password".path;
      type = "primary";
    };
  };
}
