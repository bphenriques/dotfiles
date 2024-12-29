{ pkgs, config, lib, ... }:

# TODO: https://github.com/Misterio77/nix-config/blob/main/home/gabriel/features/games/steam.nix
with lib;
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

  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;

      extraCompatPackages = with pkgs; [
        proton-ge-bin
      ];
    };

    # Steam's microcompositor that gives extra scaling features (https://github.com/ValveSoftware/gamescope).
    # - Steam: Right-click game - Properties - Launch options: gamescope -- %command% (example)
    # - Lutris: General Preferences - Enable gamescope
    gamescope.enable = true;
    gamescope.capSysNice = true;      # Ensure niceness is lower to increased priority.

    # Improve performance (https://github.com/FeralInteractive/gamemode):
    # - Steam: Right-click game - Properties - Launch options: gamemoderun %command%
    # - Lutris: General Preferences - Enable Feral GameMode
    # - Global options - Add Environment Variables: LD_PRELOAD=/nix/store/*-gamemode-*-lib/lib/libgamemodeauto.so
    gamemode.enable = true;
    gamemode.enableRenice = true;   # Ensure niceness is lower to increased priority.
  };

  environment.systemPackages = with pkgs; [
    steam-desktop-item  # Steam desktop item
  ];
}


