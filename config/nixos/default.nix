{ pkgs, ... }:
{
  imports = [ ../common.nix ];

  nix = {
    settings.auto-optimise-store   = true;  # Optimise the store when building.
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # Ensure we have at least 5GiB always available. Less than that and my system gets unstable.
    extraOptions = "min-free = ${toString (5 * 1024 * 1024 * 1024)}";
  };

  boot = {
    tmp.cleanOnBoot = true; # Not enabling useTmpfs despite having enough RAM. Might consider it.
    kernelParams = [
      "boot.shell_on_fail" # allows for root shell if failure to boot
    ];
  };

  # Network
  networking.networkmanager.enable = true;

  # Localization
  time.timeZone = "Europe/Lisbon";
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "pt_PT.UTF-8";
      LC_IDENTIFICATION = "pt_PT.UTF-8";
      LC_MEASUREMENT = "pt_PT.UTF-8";
      LC_MONETARY = "pt_PT.UTF-8";
      LC_NAME = "pt_PT.UTF-8";
      LC_NUMERIC = "pt_PT.UTF-8";
      LC_PAPER = "pt_PT.UTF-8";
      LC_TELEPHONE = "pt_PT.UTF-8";
      LC_TIME = "pt_PT.UTF-8";
    };
  };

  environment.systemPackages = let
    filesystems = [ pkgs.exfat pkgs.ntfs3g ]; # Suport exFAT and NTFS formatted drives (pendisks + external disks)
    hardware    = [
      pkgs.powertop   # Check what is consuming too much energy
      pkgs.usbutils   # USB utilities
    ];
  in filesystems ++ hardware;

  programs.fish.enable = true;  # System level.
  services.fwupd.enable = true; # Updates firmwares: `fwupdmgr`

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
