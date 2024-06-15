{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.dotfiles.profiles.gaming;

  # Script to run proton on a static prefix (because doesn't really matter for ad-hoc runs)
  # TODO: Avoid using steam-run. Alternative is to use buildFHSEnv by-hand (if I understood correctly...)
  proton-run = pkgs.writeShellApplication {
    name = "proton-run";
    runtimeInputs = with pkgs; [ proton-ge-custom steam-run ];
    text = ''
      if [ "$#" -gt 0 ]; then
        STEAM_COMPAT_DATA_PATH="${cfg.protonDefaultPrefixDir}" \
          STEAM_COMPAT_CLIENT_INSTALL_PATH="${cfg.protonDefaultPrefixDir}" \
          ${pkgs.steam-run}/bin/steam-run ${proton-ge-custom}/bin/proton run "$@"
      fi
    '';
  };

  proton-run-desktop-launcher =
    (pkgs.makeDesktopItem {
      exec = "${proton-run}/bin/proton-run %f";
      name = "Proton Launcher";
      type = "Application";
      desktopName = "Proton Launcher";
      noDisplay = false;                # FIXME: Switch to true temporary so that it appears on right-click?
      categories = [ "Utility" ];
      icon = "wine";
      mimeTypes = ["application/x-ms-dos-executable" "application/x-msi" "application/x-ms-shortcut"];
    });
in
{
  options.dotfiles.profiles.gaming = with types; {
    enable = mkOption {
      type = bool;
      default = false;
      description = mdDoc ''Whether to enable gaming profile.'';
    };
    emulationDir = mkOption {
      type = nullOr str;
      default = null;
    };
    protonDefaultPrefixDir = mkOption {
      type = nullOr  str;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    hardware = {
      opengl = {
        enable = true;
        driSupport = true;      # Installs Mesa and provides vulcan support.
        driSupport32Bit = true; # Enable OpenGL 32bit programs for Wine when using a 64bit system.
      };
      steam-hardware.enable = true;
    };

    # Tweaks
    boot = {
      kernel.sysctl."vm.max_map_count" = "2147483642";        # https://wiki.archlinux.org/title/gaming#Increase_vm.max_map_count
      kernelParams = [ "tsc=reliable" "clocksource=tsc" ];    # https://wiki.archlinux.org/title/gaming#Improve_clock_gettime_throughput
    };
    systemd.extraConfig = "DefaultLimitNOFILE=1048576"; # Proton Games - Ref: https://github.com/zfigura/wine/blob/esync/README.esync
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
        remotePlay.openFirewall = true;                 # Open ports in the firewall for Steam Remote Play
        dedicatedServer.openFirewall = true;            # Open ports in the firewall for Source Dedicated Server
        localNetworkGameTransfers.openFirewall = true;  # Allow transfer games between my systems
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

    environment.systemPackages = with pkgs; [
      heroic-unwrapped  # Epic games / GoG
      vulkan-tools      # Vulcan tester

      # Proton
      protonup-qt       # Manage Proton versions
      protontricks      # Install utility within proton
    ] ++ lib.options cfg.protonDefaultPrefixDir != null [
      proton-run        # Run .exe from termional
      proton-run-desktop-launcher
    ] ++ lib.options cfg.emulationDir != null [
      # Emulation
      # TODO: Bios in XDG_CONFIG_DIR/retroarch/system
      # TODO: emulationstation  # Does not as it needs additional configuration: perhaps https://github.com/juliosueiras-nix/nix-emulationstation/blob/master/modules/emulationstation/default.nix?
      (retroarch.override { # more: https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/emulators/retroarch/cores.nix
        cores = with libretro; [
          genesis-plus-gx # Megadrive
          snes9x          # SNES
          swanstation     # PSX
          mgba            # GBA
          desmume         # NDS
          dosbox-pure     # DOS
          prboom          # Doom
          fbneo           # Most Arcade games
          flycast         # Dreamcast
          gambatte
          #N64 - Mupen64Plus-Next with Parallel-RDP - Mupen64Plus-Next GLES3 with GlideN64
          # bsnes
          # mesen
          # PPSSPP
        ];
      })
      mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
      maxcso      # To convert to CSO
    ];
  };
}
