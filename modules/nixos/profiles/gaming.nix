{ pkgs, config, lib, ... }:

# TODO explore...
# https://github.com/Emiller88/dotfiles/blob/master/modules/desktop/gaming/steam.nix
# https://github.com/the-argus/nixsys/blob/main/modules/home-manager/gaming/default.nix#LL25C9-L25C22
# https://github.com/danderson/homelab
# https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/nixos/programs/steam/default.nix

with lib;
let
  cfg = config.custom.profiles.gaming;
  # Script to run proton on a static prefix (because doesn't really matter for ad-hoc runs)
  # TODO: Avoid using steam-run. Alternative is to use buildFHSEnv by-hand (if I understood correctly...)
  proton-run = pkgs.writeShellApplication {
    name = "proton-run";
    runtimeInputs = with pkgs; [ nur.repos.ataraxiasjel.proton-ge steam-run ];
    text = ''
      if [ "$#" -gt 0 ]; then
        STEAM_COMPAT_DATA_PATH="${cfg.defaultProtonDir}" \
          STEAM_COMPAT_CLIENT_INSTALL_PATH="${cfg.defaultProtonDir}" \
          ${pkgs.steam-run}/bin/steam-run ${pkgs.nur.repos.ataraxiasjel.proton-ge}/bin/proton run "$@"
      fi
    '';
  };

  proton-run-desktop-launcher =
    (pkgs.makeDesktopItem {
      exec = "proton-run %f";
      name = "Proton Launcher";
      type = "Application";
      desktopName = "Proton Launcher";
      categories = [ "Utility" "Game" ];
      icon = "wine";
      mimeTypes = ["application/x-ms-dos-executable" "application/x-msi" "application/x-ms-shortcut"];
    });

/*  steam-desktop-item = (makeDesktopItem {
    name = "steam";
    desktopName = "Steam";
    icon = "steam";
    exec = "steam";
    terminal = false;
    mimeTypes = ["x-scheme-handler/steam"];
    categories = [ "Network" "FileTransfer" "Game" ];
  });*/
in
{

  options.custom.profiles.gaming = with types; {
    enable = mkOption {
      type = bool;
      default = false;
      description = mdDoc ''Whether to set-up gaming profile.'';
    };

    defaultProtonDir = mkOption {
      type = str;
      default = null;
      description = mdDoc ''Default location of ad-hoc proton'';
    };
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
        remotePlay.openFirewall = true;       # Open ports in the firewall for Steam Remote Play
        dedicatedServer.openFirewall = true;  # Open ports in the firewall for Source Dedicated Server
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
    };
#        services.udev.extraRules = ''
#          KERNEL=="uinput", SUBSYSTEM=="misc", OPTIONS+="static_node=uinput", TAG+="uaccess"
#        '';


    modules.programs.lutris.enable = true;

    environment.systemPackages = with pkgs; [
      heroic-unwrapped  # Epic games / GoG
      vulkan-tools      # Vulcan tester

      # Proton
      protonup-qt       # Manage Proton versions
      protontricks      # Install utility within proton
      proton-run        # Run .exe from termional
      proton-run-desktop-launcher
    ];
  };
}
