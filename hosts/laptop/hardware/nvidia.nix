_:
{
  services.xserver.videoDrivers = [ "nvidia" ]; # Load nvidia driver for Xorg and Wayland
  hardware.graphics = {
    enable = true;
    enable32Bit = true; # Required for 32-bit Vulkan/OpenGL (Proton, Wine, umu-launcher, Heroic)
  };
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

      # Laptop is often plugged in and treated as a desktop, so keep both GPUs always on via PRIME sync.
      sync.enable = true;
      offload.enable = false;
      offload.enableOffloadCmd = false;
    };
  };
}
