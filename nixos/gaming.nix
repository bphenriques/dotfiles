{ pkgs, lib, config, ... }:
{
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true; # Enable OpenGL 32bit programs for Wine when using a 64bit system.
    };
    steam-hardware.enable = true;
  };

  boot = {
    kernel.sysctl."vm.max_map_count" = "2147483642";        # https://wiki.archlinux.org/title/gaming#Increase_vm.max_map_count
    kernelParams = [ "tsc=reliable" "clocksource=tsc" ];    # https://wiki.archlinux.org/title/gaming#Improve_clock_gettime_throughput
  };

  systemd.extraConfig = "DefaultLimitNOFILE=1048576"; # Proton Games - Ref: https://github.com/zfigura/wine/blob/esync/README.esync

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;       # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true;  # Open ports in the firewall for Source Dedicated Server
  };
  modules.services.sunshine.enable = true;
  modules.programs.lutris.enable = true;


  environment.systemPackages = with pkgs; [
    gamemode          # Improve performance.
    heroic-unwrapped  # Epic games / GoG
    protonup-qt       # List Proton
  ];
}
