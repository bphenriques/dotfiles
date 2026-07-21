{ pkgs, lib, config, self, ... }:
let
  fleetHosts = config.custom.fleet.lan.hosts
    // lib.concatMapAttrs (_: guests: guests) config.custom.fleet.microvms;
in
{
  imports = [ ../settings.nix ./shell.nix ];

  users.mutableUsers = false;

  # Nix
  nix.registry.nixpkgs.flake = self.inputs.nixpkgs; # Pin nixpkgs registry to flake input: nix shell nixpkgs#hello
  nix.gc = {
    automatic = true;
    dates = "monthly";
    options = "--delete-older-than 30d";
  };

  # Boot
  boot.tmp.cleanOnBoot = true; # Not enabling useTmpfs despite having enough RAM. Might consider it.
  system.nixos.label = let date = self.lastModifiedDate or "00000000000000";
  in "${builtins.substring 2 6 date}-${builtins.substring 8 6 date}"; # Label visible in the boot menu. Format: YYMMdd-HHmmss (e.g. 250415-194532)

  # Networking
  # Invert { hostname = ip; } to { ip = [hostnames]; } for /etc/hosts
  networking.hosts = lib.foldlAttrs (acc: name: ip: acc // { ${ip} = (acc.${ip} or [ ]) ++ [ name ]; }) { } fleetHosts;
  networking.firewall.enable = true;

  # Security
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
      AllowAgentForwarding = false;
      AllowTcpForwarding = false;
      MaxAuthTries = 3;
      LoginGraceTime = "30s";
    };
  };
  services.journald.extraConfig = ''
    MaxRetentionSec=1month
    SystemMaxUse=1G
  '';
  security.sudo.extraConfig = "Defaults lecture=never";

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = lib.genAttrs [
      "LC_ADDRESS" "LC_IDENTIFICATION" "LC_MEASUREMENT" "LC_MONETARY"
      "LC_NAME" "LC_NUMERIC" "LC_PAPER" "LC_TELEPHONE" "LC_TIME"
    ] (_: "pt_PT.UTF-8");
  };

  # Disabled defaults
  programs.nano.enable = false;
  documentation.nixos.enable = false;  # Disable generating NixOS configuration options
}
