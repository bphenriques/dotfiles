{ pkgs, ... }:
{
  nix = {
    optimise.automatic = true; # Sets up a systemd timer that regularly goes over all paths and optimises them. Can't enable it on darwin: https://github.com/NixOS/nix/issues/7273
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  hardware.pulseaudio.enable = false;  # Disable PulseAudio: https://nixos.wiki/wiki/PulseAudio
  security.rtkit.enable = true;        # Recommended for pipewire
  services.pipewire = {
    enable = true;                     # Enable pipewire
    alsa.enable = true;                # For better compatibility
    alsa.support32Bit = true;          # For better compatibility
    pulse.enable = true;               # For better compatibility
    wireplumber.enable = true;         # Audio routing policy if I understood correctly.
  };

  # Network
  networking = {
    networkmanager.enable = true;
    extraHosts = ''
      192.168.1.1     router
      192.168.68.68   pi-zero
      192.168.68.53   home-nas
    '';
  };

  # Input
  services.xserver = {
    xkb.layout = "us";
    xkb.variant = "";
    xkb.options = "caps:ctrl_modifier";   # Replace caps-lock for Ctrl

    excludePackages = [ pkgs.xterm ];
  };

  # Programs
  programs.fish.enable = true;  # System level: source vendor's completions or functions.
  programs.partition-manager.enable = true;

  # Suport exFAT and NTFS formatted drives (pendisks + external disks)
  environment.systemPackages = with pkgs; [
    exfat
    ntfs3g
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

  # Disabling some defaults
  programs.command-not-found.enable = false;
  programs.nano.enable = false;
}
