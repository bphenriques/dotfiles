{ lib, config, ... }:
let
  fleetHosts = config.fleet.lan.hosts // lib.concatMapAttrs (_: guests: guests) config.fleet.microvms;
in
{
  imports = [
    ./base.nix
    ./cli.nix
  ];

  # A count floor so a quiet week cannot leave boot counting without a fallback generation.
  programs.nh.clean = {
    enable = true;
    dates = "weekly";
    extraArgs = "--keep 5 --keep-since 7d --no-direnv";
  };
  nix = {
    settings = {
      min-free = 10 * 1024 * 1024 * 1024;  # Start GC if we are under 10 GiB free
      max-free = 50 * 1024 * 1024 * 1024;  # ...until 50 GiB is free
      trusted-users = [ "bphenriques" ];
    };
  };

  # Invert { hostname = ip; } to { ip = [hostnames]; } for /etc/hosts.
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
