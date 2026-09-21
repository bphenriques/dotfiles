{ lib, config, ... }:
let
  fleetHosts = config.custom.fleet.lan.hosts // lib.concatMapAttrs (_: guests: guests) config.custom.fleet.microvms;
in
{
  imports = [
    ./base.nix
    ./cli.nix
  ];

  # Nix
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    settings = {
      min-free = 10 * 1024 * 1024 * 1024;  # Start GC if we are under 10 GiB free
      max-free = 50 * 1024 * 1024 * 1024;  # ...until 50 GiB is free
    };
  };

  # Networking
  # Invert { hostname = ip; } to { ip = [hostnames]; } for /etc/hosts
  networking.hosts = lib.foldlAttrs (acc: name: ip: acc // { ${ip} = (acc.${ip} or [ ]) ++ [ name ]; }) { } fleetHosts;

  # Misc
  services.journald.settings.Journal = {
    MaxRetentionSec = "1month";
    SystemMaxUse = "1G";
  };

  i18n.extraLocaleSettings = lib.genAttrs [
    "LC_ADDRESS" "LC_IDENTIFICATION" "LC_MEASUREMENT" "LC_MONETARY"
    "LC_NAME" "LC_NUMERIC" "LC_PAPER" "LC_TELEPHONE" "LC_TIME"
  ] (_: "pt_PT.UTF-8");
}
