{ pkgs, config, lib, ... }:

with lib;
let
  cfg = config.custom.profiles.gaming;
in
{
  options.custom.profiles.gaming = with types; {
    enable = mkEnableOption "gaming profile";
  };

  config = mkIf cfg.enable {
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

    hardware = {
      graphics.enable = true;
      graphics.enable32Bit = true;
      steam-hardware.enable = true;
    };

    programs = {
      steam = {
        enable = true;
        remotePlay.openFirewall = true;
        dedicatedServer.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
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

    services.sunshine = {
      enable = true;
      openFirewall = true;
      capSysAdmin = true;
      applications = {
        env = {
          PATH = "$(PATH):$(HOME)/.local/bin";
        };
        apps = [
          {
            name = "Desktop";
            image-path = "desktop.png";
          }
          {
             name = "Steam Big Picture";
             output = "/tmp/sunlight-steam.txt";
             detached = ["${pkgs.util-linux}/bin/setsid ${pkgs.steam}/bin/steam steam://open/bigpicture"];
             image-path = "steam.png";
          }
        ];
      };
    };
    # Required to simulate input
    services.udev.extraRules = ''
      # Sunshine
      KERNEL=="uinput", SUBSYSTEM=="misc", OPTIONS+="static_node=uinput", TAG+="uaccess"
    '';

    environment.systemPackages = with pkgs; [
      heroic  # Epic games / GoG
    ];
  };
}

# https://github.com/azuwis/nix-config/blob/d29d918097e5da916be5762255fb418c657860bc/nixos/sunshine/home.nix#L17
# https://github.com/lucasew/nixcfg/blob/90505958b98379ac19d7c952a27bd7bce8714816/nix/nodes/gui-common/sunshine.nix#L9
# https://github.com/aostanin/nixos-config/blob/c73238deae8538bc6cd0b885f108255e60c60e94/nixos/modules/headless-gaming.nix#L12

# Enable using:
# services.sunshine.enable = true;
# Get Service Status
# systemctl --user status sunshine
# get logs
# journalctl --user -u sunshine --since "2 minutes ago"