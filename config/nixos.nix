{ pkgs, ... }:

{
  nix = {
    optimise.automatic = true; # Sets up a systemd timer that regularly goes over all paths and optimises them
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  # Audio - Pipewire over ALSA and PulseAudio: https://nixos.wiki/wiki/PipeWire
  # FIXME: https://github.com/bbigras/nix-config/blob/master/hardware/sound.nix
  # https://github.com/NixOS/nixpkgs/blob/a1521bc2d16d34067ee45aac75387eaa63f638c0/nixos/doc/manual/release-notes/rl-2411.section.md?plain=1#L369C10-L369C16
  hardware.pulseaudio.enable = false;  # Disable PulseAudio: https://nixos.wiki/wiki/PulseAudio
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # For better compatibility
    alsa.support32Bit = true;          # For better compatibility
    pulse.enable = true;               # For better compatibility
  };

  # Network
  networking.networkmanager.enable = true;

  # Input
  services.xserver = {
    xkb.layout = "us";
    xkb.variant = "";
    xkb.options = "caps:ctrl_modifier";   # Replace caps-lock for Ctrl

    excludePackages = [ pkgs.xterm ];
  };

  # Fonts (system-wide)
  fonts.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
  ];

  # Programs
  programs.fish.enable = true;                          # System level: source vendor's completions or functions.
  programs.partition-manager.enable = true;
  environment.systemPackages = with pkgs; [
    p7zip     # Zip/Unzip that supports all the formats I need
    baobab    # Disk Space Analyser

    museeks   # Audio
    amberol   # Another Audio music player. Yet another alternative: https://github.com/jeffvli/sonixd
    vlc       # Video
    qbittorrent

    # File Management
    filezilla

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

  services.journald.extraConfig = ''
    MaxRetentionSec=1month
    SystemMaxUse=1G
  '';

  # Security: https://github.com/AntonHakansson/nixos-config/blob/main/modules/core/default.nix#L79
  security.sudo.extraConfig = "Defaults lecture=never";

  # TODO: Nice boot themes: https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/initrd/default.nix#L63C12-L63C20. Is this related? https://github.com/jordanisaacs/dotfiles/blob/master/modules/system/kernel/default.nix
  # Disabling some defaults
  programs.command-not-found.enable = false;
  programs.nano.enable = false;
}
