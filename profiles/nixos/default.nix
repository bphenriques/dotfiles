{ pkgs, lib, self, ... }:
{
  imports = [ ../common.nix ];

  # Label visible in the boot menu. Format: YYMMdd-HHmmss (e.g. 250415-194532)
  system.nixos.label = let
    date = self.lastModifiedDate or "00000000000000";
  in "${builtins.substring 2 6 date}-${builtins.substring 8 6 date}";

  # Pin nixpkgs registry to flake input (replaces nix-channel for flake workflows)
  # Allows: nix shell nixpkgs#hello, nix run nixpkgs#cowsay, etc.
  nix.registry.nixpkgs.flake = self.inputs.nixpkgs;

  boot.tmp.cleanOnBoot = true; # Not enabling useTmpfs despite having enough RAM. Might consider it.

  networking.firewall.enable = true;
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

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = lib.genAttrs [
      "LC_ADDRESS" "LC_IDENTIFICATION" "LC_MEASUREMENT" "LC_MONETARY"
      "LC_NAME" "LC_NUMERIC" "LC_PAPER" "LC_TELEPHONE" "LC_TIME"
    ] (_: "pt_PT.UTF-8");
  };

  programs.fish.enable = true;  # System level.

  # Disabling some defaults
  programs.command-not-found.enable = false;
  programs.nano.enable = false;

  # Security
  services.journald.extraConfig = ''
    MaxRetentionSec=1month
    SystemMaxUse=1G
  '';
  security.sudo.extraConfig = "Defaults lecture=never";

  # Misc
  home-manager.useGlobalPkgs    = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages  = true;   # Install packages defined in home-manager.
  documentation.nixos.enable    = false;  # Disable generating NixOS configuration options
}
