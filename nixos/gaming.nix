{ pkgs, lib, nixpkgs-unstable, config, ... }:
{
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true; # Enable OpenGL 32bit programs for Wine when using a 64bit system.
  };

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  modules.programs.lutris.enable = true;
  systemd.extraConfig = "DefaultLimitNOFILE=1048576"; # Proton Games - Ref: https://github.com/zfigura/wine/blob/esync/README.esync
  environment.systemPackages = with pkgs; [
    sunshine          # Moonlight streaming - learn how to override... https://nixos.org/guides/nix-pills/nixpkgs-overriding-packages.html
    heroic-unwrapped  # Epic games / GoG
    protonup-qt
    # nixpkgs-unstable.pkgs.proton-caller  # Run .exe from terminal
  ];

  # cd /tmp && wget https://github.com/GloriousEggroll/proton-ge-custom/releases/download/GE-Proton7-53/GE-Proton7-53.tar.gz && tar -C /home/$USER/.local/share/lutris/runners/wine -xf GE-Proton7-53.tar.gz GE-Proton7-53/files/ --one-top-level --strip=2
}
