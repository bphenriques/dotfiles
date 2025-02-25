{ pkgs, self, ... }:
{
  imports = [
    ./login-manager.nix # Login screen
    ./audio.nix
    ./compositor.nix    # Sets the compositor globally.
    ./docker.nix        # Virtualization
    ./steam.nix         # Gaming: store + library
    ./gamemode.nix      # Gaming: nice to have.
    ./gamescope.nix     # Gaming: Micro-compositor
    ./nautilus.nix      # Ideally I wish it was terminal based but xdg-desktop-portal-termfilechooser seems too old to consider
    # ./sunshine.nix    # Still doesnt work.
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
}
