{ pkgs, lib, network-devices, ... }:
{
  imports = [
    ./programs.nix
    ./services.nix
    ./display-manager.nix
    ./wayland.nix

    # Choose one file browser
    ./thunar.nix
    ./nautilus.nix
  ];

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  home-manager.useGlobalPkgs   = true;   # Use pkgs set within nixpkgs.
  home-manager.useUserPackages = true;   # Install packages defined in home-manager.

  # Not enabling useTmpfs despite having enough RAM. Might consider it.
  boot.tmp.cleanOnBoot = true;

  # Audio
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

  security.sudo.extraConfig = "Defaults lecture=never";
}
