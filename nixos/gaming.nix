{ pkgs, lib, config, ... }:

# TODO explore...
# https://github.com/Emiller88/dotfiles/blob/master/modules/desktop/gaming/steam.nix
# https://github.com/the-argus/nixsys/blob/main/modules/home-manager/gaming/default.nix#LL25C9-L25C22
# https://github.com/danderson/homelab

let
  # Rudimentary script to launch ad-hoc Windows applications
  # TODO: Avoid using steam-run. Alternative is to use buildFHSEnv by-hand (if I understood correctly...)
  proton-run = pkgs.writeShellApplication {
    name = "proton-run";
    runtimeInputs = with pkgs; [ proton-ge-custom steam-run ];
    text = ''
      STEAM_COMPAT_DATA_PATH="${config.user.protonDefaultPrefixDir}" \
        STEAM_COMPAT_CLIENT_INSTALL_PATH="${config.user.protonDefaultPrefixDir}" \
        ${pkgs.steam-run}/bin/steam-run ${pkgs.proton-ge-custom}/bin/proton run "$@"
    '';
  };
in
{
  hardware = {
    opengl = {
      enable = true;
      driSupport = true;      # Installs Mesa and provides vulcan support.
      driSupport32Bit = true; # Enable OpenGL 32bit programs for Wine when using a 64bit system.
    };
    steam-hardware.enable = true;
  };

  boot = {
    kernel.sysctl."vm.max_map_count" = "2147483642";        # https://wiki.archlinux.org/title/gaming#Increase_vm.max_map_count
    kernelParams = [ "tsc=reliable" "clocksource=tsc" ];    # https://wiki.archlinux.org/title/gaming#Improve_clock_gettime_throughput
  };

  systemd.extraConfig = "DefaultLimitNOFILE=1048576"; # Proton Games - Ref: https://github.com/zfigura/wine/blob/esync/README.esync

  programs = {
    steam = {
      enable = true;
      remotePlay.openFirewall = true;       # Open ports in the firewall for Steam Remote Play
      dedicatedServer.openFirewall = true;  # Open ports in the firewall for Source Dedicated Server
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
  modules.services.sunshine.enable = true;
  modules.programs.lutris.enable = true;

#  user.packages = with pkgs; [
#    (makeDesktopItem {
#      name = "steam";
#      desktopName = "Steam";
#      icon = "steam";
#      exec = "steam";
#      terminal = "false";
#      categories = [ "Network" "FileTransfer" "Game" ];
#    })
#  ];

  environment.systemPackages = with pkgs; [
    heroic-unwrapped  # Epic games / GoG
    vulkan-tools      # Vulcan tester

    # Proton
    protonup-qt       # Manage Proton versions
    protontricks      # Install utility within proton
    proton-run        # Run .exe from termional
  ];
}
