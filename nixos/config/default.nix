{ pkgs, ... }:

{
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
  # TODO: Review these packages: https://github.com/diegofariasm/yggdrasil/blob/main/modules/nixos/hardware/audio.nix

  # Network
  networking.networkmanager.enable = true;

  # Input
  services.xserver = {
    xkb.layout = "us";
    xkb.variant = "";
    xkb.options = "caps:ctrl_modifier";   # Replace caps-lock for Ctrl
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

    museeks   # Audio
    amberol   # Another Audio music player. Yet another alternative: https://github.com/jeffvli/sonixd
    vlc       # Video

    # Filesystems (will I ever need this?)
    exfat     # Windows drives
    ntfs3g    # Windows drives
    hfsprogs  # MacOS drives
  ];

  # Services
  services.qbittorrent.enable = true;

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

  # Disabling some defaults
  programs.command-not-found.enable = false;
  programs.nano.enable = false;
}
