{ pkgs, lib, network-devices, ... }:
{
  nix = {
    settings.auto-optimise-store   = true;  # Optimise the store when building.
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  boot.kernelParams = [
    "boot.shell_on_fail" # allows for root shell if failure to boot
  ];

  # Not enabling useTmpfs despite having enough RAM. Might consider it.
  boot.tmp.cleanOnBoot = true;

  # Network
  networking = {
    networkmanager.enable = true;
    extraHosts = ''
      ${network-devices.home-nas.hostname}  home-nas
      ${network-devices.pi-zero.hostname}   pi-zero
      ${network-devices.rg353m.hostname}    rg353m
    '';
  };

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

  programs.fish = {
    enable = true;                  # System level.
    vendor.functions.enable = true; # Ensure completions/functions are automatically set.
  };

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

  # Disable generating NixOS configuration options
  documentation.nixos.enable = false;

  # Home Settings
  home-manager.useGlobalPkgs   = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages = true;   # Install packages defined in home-manager.
}
