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
  home-manager.useGlobalPkgs   = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages = true;   # Install packages defined in home-manager.

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

  # Input - More on https://wiki.archlinux.org/title/Xorg/Keyboard_configuration
  services.xserver = {
    #exportConfiguration = true;  # Do I need this?
    xkb.layout = "us,pt";   # localectl list-x11-keymap-layouts and
    xkb.variant = "euro,";  # localectl list-x11-keymap-variants us
    xkb.options = builtins.concatStringsSep " " [
      "caps:ctrl_modifier"    # Replace caps-lock for Ctrl
      "grp:win_space_toggle"  # Switch keyboard layouts
    ];

    excludePackages = [ pkgs.xterm ];
  };

  # Programs
  programs.fish.enable = true;  # System level/
  programs.fish.vendor.functions.enable = true; # Ensure completions/functions are automatically set.
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

  # To install or run some programs, it is easier to this way. The exception.
  # Follow with: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  services.flatpak.enable = true;

  # Disabling some defaults
  programs.command-not-found.enable = false;
  programs.nano.enable = false;
}
