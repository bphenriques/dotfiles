{ pkgs, lib, config, ... }:

{
  # Display environment
  services.xserver = {
    enable = true;                        # X11 because setting up Wayland is more complicated than it is worth for me.
    displayManager.sddm.enable = true;    # SDDM login page.
    desktopManager.plasma5.enable = true; # Plasma environment.
  };

  # Network
  networking.networkmanager.enable = true;
  user.extraGroups = ["networkmanager"];

  # Input
  services.xserver = {
    layout = "us";
    xkbVariant = "";
    xkbOptions = "caps:ctrl_modifier";   # Replace caps-lock for Ctrl
  };

  # Audio - Pipewire over ALSA and PulseAudio: https://nixos.wiki/wiki/PipeWire
  sound.enable = false;                # Disable ALSA (it is used as a low-level API for pipewire): https://nixos.wiki/wiki/ALSA
  hardware.pulseaudio.enable = false;  # Disable PulseAudio: https://nixos.wiki/wiki/PulseAudio
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # For better compatibility
    alsa.support32Bit = true;          # For better compatibility
    pulse.enable = true;               # For better compatibility
  };

  # Fonts (system-wide)
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];

  # Programs
  programs.partition-manager.enable = true;
  environment.systemPackages = with pkgs; [
    p7zip     # Zip/Unzip that supports all the formats I need

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
    ./gaming.nix
    ./media.nix
    ./development.nix
    ./internet.nix
  ];
}
