{ pkgs, lib, config, ... }:

{
  # Set zsh shell
  users.defaultUserShell = pkgs.zsh;
  environment.shells = with pkgs; [ zsh ];

  # Display environment
  services.xserver = {
    enable = true;                        # X11 because setting up Wayland is more complicated than it is worth for me.
    displayManager.sddm.enable = true;    # SDDM login page.
    desktopManager.plasma5.enable = true; # Plasma environment.
  };

  # Network
  networking.networkmanager.enable = true;
  user.extraGroups = ["networkmanager" "banana"];

  # Input
  services.xserver.xkbOptions = "caps:ctrl_modifier";   # Replace caps-lock for Ctrl

  # Sound
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true; # Recommended for pulseaudio.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Basic programs
  programs.partition-manager.enable = true;
  environment.systemPackages = with pkgs; [
    ark             # Zip/Unzip

    # Filesystems (will I use this? doesnt hurt to have this)
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
  ];
}
