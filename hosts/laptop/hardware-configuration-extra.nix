{ pkgs, config, ... }:

# Mirrored from https://github.com/NixOS/nixos-hardware/blob/master/lenovo/legion/16aph8/default.nix
# Based on Nix-Hardware (see https://github.com/wochap/nix-config/blob/main/hosts/glegion/hardware-configuration.nix)
# TODO: Check AMDVLK https://github.com/NixOS/nixpkgs/pull/318175
{
  boot.kernelParams = [
    "amd_pstate=active"   # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html. On recent AMD CPUs this can be more energy efficient.
    "amdgpu.sg_display=0" # Fixes flickring or stays white (https://wiki.archlinux.org/title/AMDGPU)
  ];
  boot.initrd.kernelModules = [ "amdgpu" ];
  hardware.graphics.extraPackages = with pkgs; [
    vaapiVdpau  # Nvidia
  ];

  hardware.graphics.extraPackages32 = with pkgs; [
    driversi686Linux.amdvlk
  ];

  hardware.nvidia = {
    modesetting.enable = true;
    nvidiaSettings = true;
    powerManagement.enable = true;
    powerManagement.finegrained = true;
    open = false;
    prime = {
      offload.enable = true;
      offload.enableOffloadCmd = true;
      amdgpuBusId = "PCI:5:0:0";
      nvidiaBusId = "PCI:1:0:0";
    };
  };

  environment.systemPackages = with pkgs; [
    lenovo-legion # TODO: Probabilly can need to set fn-lock, battery
    (nvtopPackages.nvidia.override { amd = true; })  # Top but for GPUs
    #amdgpu_top
  ];

  # Avoid issues with modesetting causing blank screen
  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  # Bluetooth
  # TODO: https://github.com/bbigras/nix-config/blob/master/hardware/bluetooth.nix
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true; # Power up the bluetooth controller on boot
  };
  services.blueman.enable = true;

  # SSD
  services.fstrim.enable = true;  # Trim SSD because for some reason is not a default :shrug:

  # Run zramctl to check how good memory is compressed
  zramSwap.enable = true;

  # FIXME:
  #  systemd.network.wait-online.enable = false;
  #  boot.initrd.systemd.network.wait-online.enable = false;
  #  networking.wireless.dbusControlled = false;

  # FIXME: Battery stuff: https://github.com/bbigras/nix-config/blob/master/hosts/laptop/default.nix#L173

  # Peripherals
  hardware.keyboard.zsa.enable = true;  # Required to flash or use live training online
  services.libinput = {
    enable = true;
    touchpad.naturalScrolling = true;
    touchpad.tapping = true;
  };
}
