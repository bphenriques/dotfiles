{ pkgs, ... }:
{
  services.fwupd.enable = true; # Updates firmwares: `fwupdmgr`

  # To install or run some programs, it is easier to this way.
  # Follow with: flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
  services.flatpak.enable = true;
  services.journald.extraConfig = ''
    MaxRetentionSec=1month
    SystemMaxUse=1G
  '';
}
