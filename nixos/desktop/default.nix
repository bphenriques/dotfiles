{ pkgs, ... }:
{
  imports = [
    ./audio.nix
    ./steam.nix       # Gaming
    ./gamemode.nix
    ./gamescope.nix
    ./docker.nix
    # ./sunshine.nix # TODO: does not work
  ];

  # Boot
  boot.kernelParams = [
    "boot.shell_on_fail"  # Use shell when booting fails
  ];

  # Graphics
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  services = {
    flatpak.enable = true;        # Easier to run some programs. Setup afterwards: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  };

  # Other
  environment.systemPackages = [
    pkgs.konsole   # Boring terminal in case something goes wrong

    # Personalization
    pkgs.morewaita-icon-theme
    pkgs.adwaita-icon-theme
    pkgs.qogir-icon-theme
  ];
}
