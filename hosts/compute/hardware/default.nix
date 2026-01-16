{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
  ];

  # Misc drivers
  hardware.enableRedistributableFirmware = true;

  # Peripherals
  boot.extraModprobeConfig = "options hid_apple fnmode=2 swap_opt_cmd=0"; # Nuphy Air75 (check the flags with `modinfo -p hid_apple`)

  # GPU
  #hardware.graphics = {
  #  enable = true;
  #  extraPackages = with pkgs; [
  #    intel-media-driver # LIBVA_DRIVER_NAME=iHD
  #    intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but good fallback)
  #    vaapiVdpau
  #    libvdpau-va-gl
  #  ];
  #};
  # TODO: disable wifi card to lower power usage.

}
