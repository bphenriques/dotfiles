{ pkgs, ... }:
{
  # Programs
  programs = {
    fish = {
      enable = true;                  # System level.
      vendor.functions.enable = true; # Ensure completions/functions are automatically set.
    };
    partition-manager.enable = true;

    # Disabling some defaults
    command-not-found.enable = false;
    nano.enable = false;
  };

  environment.systemPackages = with pkgs; [
    # Suport exFAT and NTFS formatted drives (pendisks + external disks)
    exfat
    ntfs3g

    powertop  # Check what is consuming too much energy
    usbutils  # USB utilities

    # Other
    cheese # Webcam
    # amberol? blanket?
  ];
}
