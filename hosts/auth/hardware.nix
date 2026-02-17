# Hardware configuration for auth VM (microvm)
{ lib, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.initrd.availableKernelModules = [ "virtio_pci" "virtio_scsi" "virtio_blk" "virtio_net" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  # VM doesn't need firmware
  hardware.enableRedistributableFirmware = lib.mkDefault false;

  # Minimal kernel for VM
  boot.kernelParams = [ "console=ttyS0" ];
}
