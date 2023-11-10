{ pkgs, lib, config, ... }:

{
  # Network
  networking.networkmanager.enable = true;
  user.extraGroups = ["networkmanager"];

  # Input
  services.xserver = {
    layout = "us";
    xkbVariant = "";
    xkbOptions = "caps:ctrl_modifier";   # Replace caps-lock for Ctrl
  };

  # Fonts (system-wide)
  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];

  # Programs
  programs.partition-manager.enable = true;
  environment.systemPackages = with pkgs; [
    p7zip     # Zip/Unzip that supports all the formats I need
    baobab    # Disk Space Analyser

    # Filesystems (will I ever need this?)
    exfat     # Windows drives
    ntfs3g    # Windows drives
    hfsprogs  # MacOS drives
  ];

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

  imports = [
    ./desktop-environment.nix
    ./audio.nix
    ./gaming.nix
    ./media.nix
    ./development.nix
    ./internet.nix
  ];
}
