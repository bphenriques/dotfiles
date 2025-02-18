{ pkgs, self, ... }:
{
  imports = [
    ./audio.nix
    ./steam.nix       # Gaming
    ./gamemode.nix
    ./gamescope.nix
    ./docker.nix
    # ./sunshine.nix # FIXME: does not work
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
