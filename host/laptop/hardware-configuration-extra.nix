{ pkgs, ... }:

# Mirrored from https://github.com/NixOS/nixos-hardware/blob/master/lenovo/legion/16aph8/default.nix
# Also check https://github.com/wochap/nix-config/blob/main/hosts/glegion/hardware-configuration.nix
{
  boot.initrd.kernelModules = [ "amdgpu" ];
  hardware.opengl.extraPackages = with pkgs; [
    amdvlk      # AMD
    vaapiVdpau  # Nvidia
  ];

  hardware.opengl.extraPackages32 = with pkgs; [
    driversi686Linux.amdvlk
  ];

  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    prime = {
      sync.enable = true;
      amdgpuBusId = "PCI:5:0:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  # Avoid issues with modesetting causing blank screen
  services.xserver.videoDrivers = [ "nvidia" "modesetting" ];

  # SSD
  services.fstrim.enable = true;  # Trim SSD because for some reason is not a default :shrug:
}
