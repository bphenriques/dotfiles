{ pkgs, ... }:

# Mirrored from https://github.com/NixOS/nixos-hardware/blob/master/lenovo/legion/16aph8/default.nix
# Based on Nix-Hardware (see https://github.com/wochap/nix-config/blob/main/hosts/glegion/hardware-configuration.nix)
# TODO: Check https://github.com/NixOS/nixpkgs/pull/318175
{
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [
    "amd_pstate=active"   # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html. On recent AMD CPUs this can be more energy efficient.
    "amdgpu.sg_display=0" # Fixes flickring or stays white (https://wiki.archlinux.org/title/AMDGPU)
  ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  hardware.opengl.extraPackages = with pkgs; [
    vaapiVdpau  # Nvidia
  ];

  hardware.opengl.extraPackages32 = with pkgs; [
    driversi686Linux.amdvlk
  ];

  hardware.nvidia = {
    modesetting.enable = true;
    nvidiaSettings = true;
    powerManagement.enable = true;
    powerManagement.finegrained = true;
    package = config.boot.kernelPackages.nvidiaPackages.beta;
    open = false;
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true;
      amdgpuBusId = "PCI:5:0:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  environment.systemPackages = with pkgs; [
    lenovo-legion
    # (nvtopPackages.nvidia.override { amd = true; })
    # amdgpu_top
  ];

  # Avoid issues with modesetting causing blank screen
  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  #hardware.bluetooth.enable = true;
  # SSD
  services.fstrim.enable = true;  # Trim SSD because for some reason is not a default :shrug:

  # Touchpad
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    touchpad.tapping = true;
  };
}

# This one is nice: https://github.com/iynaix/dotfiles/blob/main/nixos/impermanence.nix#L35
