_:
{
  services.xserver.videoDrivers = [ "nvidia" ]; # Load nvidia driver for Xorg and Wayland
  hardware.nvidia = {
    modesetting.enable = true;
    dynamicBoost.enable = true;
    nvidiaSettings = true;
    powerManagement.enable = true;
    open = false;
    prime = {
      # `sudo lshw -c` display to check businfo. Convert hexa to decimal, then remove leading zeroes, and replace . with ;
      amdgpuBusId = "PCI:5:0:0";
      nvidiaBusId = "PCI:1:0:0";

      # This is a desktop, therefore I am ok having both GPUs enabled and delegating the switch to PRIME
      sync.enable = true;
      offload.enable = false;
      offload.enableOffloadCmd = false;
    };
  };
}
