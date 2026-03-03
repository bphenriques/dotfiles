{ config, ... }:
{
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false; # Delay until required
    autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" ];
    };
  };

  hardware.nvidia-container-toolkit.enable = builtins.elem "nvidia" config.services.xserver.videoDrivers;
}
