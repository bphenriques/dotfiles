{ pkgs, config, lib, ... }:
let
  steam-desktop-item = (pkgs.makeDesktopItem {
    name = "steam";
    desktopName = "Steam";
    icon = "steam";
    exec = "${pkgs.steam}/bin/steam";
    terminal = false;
    mimeTypes = [ "x-scheme-handler/steam" ];
    categories = [ "Network" "FileTransfer" "Game" ];
  });
in
{
  # Tweaks
  boot = {
    kernel.sysctl."vm.max_map_count" = "2147483642";        # https://wiki.archlinux.org/title/gaming#Increase_vm.max_map_count
    kernelParams = [ "tsc=reliable" "clocksource=tsc" ];    # https://wiki.archlinux.org/title/gaming#Improve_clock_gettime_throughput
  };

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "hard";
      item = "nofile";
      value = "1048576";
    }
  ];

  programs.steam = {
    enable = true;
    extest.enable = true;
    extraCompatPackages = [ pkgs.proton-ge-bin ];
    protontricks.enable = true;
    gamescopeSession.enable = config.programs.gamescope.enable;

    # Network options
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };
  
  environment.systemPackages = [
    steam-desktop-item
  ];
}


