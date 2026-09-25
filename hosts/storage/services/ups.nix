{ config, lib, ... }:
let
  # Generated here, where upsd validates them. Clients keep their own copy in their own secret store.
  upsUsers = [
    "storage"
    "compute"
    "ai"
  ];
  upsPassword = name: config.selfhost.runtimeSecrets."ups-password-${name}".path;
  inherit (config.fleet) ups;
in
{
  selfhost.runtimeSecrets = lib.genAttrs (map (n: "ups-password-${n}") upsUsers) (_: {
    bytes = 24;
    restartUnits = [
      "upsd.service"
      "upsmon.service"
    ];
  });

  power.ups = {
    enable = true;
    mode = "netserver";
    openFirewall = false; # ../firewall.nix opens 3493 to compute only.
    ups.${ups.name} = {
      driver = "usbhid-ups";
      port = "auto";
      description = "Storage UPS";
    };
    # DHCP assigns the LAN address, so upsd cannot bind a literal one at start.
    upsd.listen = [ { address = "0.0.0.0"; } ];
    users = {
      storage = {
        passwordFile = upsPassword "storage";
        upsmon = "primary";
      };
      compute = {
        passwordFile = upsPassword "compute";
        upsmon = "secondary";
      };
      ai = {
        passwordFile = upsPassword "ai";
        upsmon = "secondary";
      };
    };
    upsmon.monitor.${ups.name} = {
      system = "${ups.name}@localhost";
      powerValue = 1;
      user = "storage";
      passwordFile = upsPassword "storage";
      type = "primary";
    };
  };
}
