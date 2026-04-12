{ lib, pkgs, config, osConfig, ... }:
let
  emulationPaths = osConfig.custom.homelab.paths.media.gaming.emulation;
  stateDir = "${config.xdg.stateHome}/retroarch";
in
lib.mkIf pkgs.stdenv.isLinux {
  programs.retroarch = {
    enable = true;
    # Cores: https://github.com/NixOS/nixpkgs/tree/master/pkgs/applications/emulators/libretro/cores
    cores = {
      genesis-plus-gx.enable = true; # Megadrive
      fceumm.enable          = true; # NES
      snes9x.enable          = true; # Snes
      swanstation.enable     = true; # PSX
      gambatte.enable        = true; # Gameboy (Color)
      mgba.enable            = true; # GBA
      desmume.enable         = true; # NDS
      dosbox-pure.enable     = true; # DOS
      prboom.enable          = true; # Doom
      fbneo.enable           = true; # Arcade
      flycast.enable         = true; # Dreamcast
    };

    # Config: https://github.com/libretro/RetroArch/blob/master/retroarch.cfg
    settings = {
      # Paths
      system_directory = emulationPaths.bios;
      rgui_browser_directory = emulationPaths.roms;
      savefile_directory = "${stateDir}/savefiles";
      savestate_directory = "${stateDir}/savestates";
      playlist_directory = "${stateDir}/playlists";

      # Saves
      savestate_auto_index = "true";
      savestate_thumbnail_enable = "true";
      sort_savefiles_enable = "true";
      sort_savestates_enable = "true";

      # Video
      video_driver = "vulkan";

      # Prevent RetroArch from overwriting managed config
      config_save_on_exit = "false";
    };
  };

  home.packages = [
    pkgs.mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
    pkgs.maxcso      # To convert to CSO
  ];
}
