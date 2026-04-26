{ config, ... }: {
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
    autoPrune = {
      enable = true;
      dates = "weekly";
      flags = [ "--all" ];
    };
  };
  hardware.nvidia-container-toolkit.enable = builtins.elem "nvidia" config.services.xserver.videoDrivers;
}