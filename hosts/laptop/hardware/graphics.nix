{ pkgs, config, lib, ... }:

# Based on:
# - https://github.com/NixOS/nixos-hardware/blob/master/lenovo/legion/16aph8/default.nix
# - https://github.com/wochap/nix-config/blob/main/hosts/glegion/hardware-configuration.nix)
{
  hardware.amdgpu.initrd.enable = true;
  hardware.amdgpu.opencl.enable = true;

  boot.kernelParams = [
    "amd_pstate=active"   # Enables the amd cpu scaling https://www.kernel.org/doc/html/latest/admin-guide/pm/amd-pstate.html. On recent AMD CPUs this can be more energy efficient.
    "amdgpu.sg_display=0" # Fixes flickring or stays white (https://wiki.archlinux.org/title/AMDGPU)
  ];

  # https://discord.com/channels/568306982717751326/1275410775271538809/1275501245079093362
  # https://github.com/NixOS/nixpkgs/blob/4f9cb71da3ec4f76fd406a0d87a1db491eda6870/nixos/modules/programs/gamescope.nix#L47
  hardware.nvidia = {
    modesetting.enable = true;
    dynamicBoost.enable = true;
    nvidiaSettings = true;
    powerManagement.enable = true;
    open = false;
    prime = {
      # Use sudo lshw -c display to check businfo. Convert hexa to decimal, remove leading zeroes, replace the . with ;
      amdgpuBusId = "PCI:5:0:0";
      nvidiaBusId = "PCI:1:0:0";

      sync.enable = true;
      offload.enable = false;
      offload.enableOffloadCmd = false;
    };
  };

  environment.systemPackages = with pkgs; [
    lenovo-legion # TODO: Probabilly can need to set fn-lock, battery
    (nvtopPackages.nvidia.override { amd = true; })  # Top but for GPUs
  ];

  # Enabled by default on plasma6. Might need to enable it by hand outside of it. See https://github.com/NixOS/nixpkgs/blob/8843893c9b842fcac17263a5700ee496e2cbee7f/nixos/modules/services/desktop-managers/plasma6.nix#L224
  # AMD has better battery life with PPD over TLP:
  # https://community.frame.work/t/responded-amd-7040-sleep-states/38101/13
  services.power-profiles-daemon.enable = true;

  # TODO: build from git main branch, for better support

  specialisation = {
    force-igpu.configuration = {
      system.nixos.tags = [ "force-igpu" ];
      hardware.nvidia = {
        powerManagement.finegrained = true;
        prime.offload.enable = lib.mkForce true;
        prime.offload.enableOffloadCmd = lib.mkForce true;
        prime.sync.enable = lib.mkForce false;
      };
    };
  };

  services.xserver.videoDrivers = [ "nvidia" ];
}
