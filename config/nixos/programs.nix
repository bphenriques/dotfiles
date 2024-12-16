{ pkgs, ... }:
{
  environment.systemPackages = let
    filesystems = [ pkgs.exfat pkgs.ntfs3g ]; # Suport exFAT and NTFS formatted drives (pendisks + external disks)
    hardware    = [
      pkgs.powertop   # Check what is consuming too much energy
      pkgs.usbutils   # USB utilities
    ];
  in filesystems ++ hardware;

  # Programs
  programs = {
    fish = {
      enable = true;                  # System level.
      vendor.functions.enable = true; # Ensure completions/functions are automatically set.
    };

    # Disabling some defaults
    command-not-found.enable = false;
    nano.enable = false;
  };
}
