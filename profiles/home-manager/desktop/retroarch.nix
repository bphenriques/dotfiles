{ lib, pkgs, config, osConfig, ... }:
let
  emulationPaths = osConfig.custom.homelab.paths.media.gaming.emulation;
  stateDir = "${config.xdg.stateHome}/retroarch";

  # Per-core config: https://docs.libretro.com/library/
  # Option key names verified against libretro source code.
  shaderPath = "${pkgs.libretro-shaders-slang}/share/libretro/shaders/shaders_slang";

  # Key: canonical core id. `displayName` must match RetroArch's core display name exactly
  # (used for per-core config/shader directory names).
  coreConfigs = {
    genesis_plus_gx = {
      displayName = "Genesis Plus GX";
      options = ''
        genesis_plus_gx_overscan = "disabled"
        genesis_plus_gx_render = "single field"
      '';
    };
    fceumm = {
      displayName = "FCEUmm";
      options = ''
        fceumm_overscan_h = "enabled"
        fceumm_overscan_v = "enabled"
      '';
    };
    swanstation = {
      displayName = "SwanStation";
      options = ''
        swanstation_GPU_ResolutionScale = "3"
        swanstation_GPU_TrueColor = "true"
        swanstation_GPU_TextureFilter = "Nearest"
        swanstation_GPU_PGXPEnable = "true"
        swanstation_GPU_PGXPCulling = "true"
        swanstation_GPU_PGXPTextureCorrection = "true"
      '';
    };
    flycast = {
      displayName = "Flycast";
      options = ''
        flycast_internal_resolution = "1920x1440"
        flycast_anistropic_filtering = "4"
        flycast_enable_rtt = "On"
      '';
    };
    desmume = {
      displayName = "DeSmuME";
      options = ''
        desmume_internal_resolution = "512x384"
        desmume_screens_layout = "top/bottom"
        desmume_screens_gap = "0"
      '';
    };
    gambatte = {
      displayName = "Gambatte";
      options = ''
        gambatte_gb_colorization = "internal"
        gambatte_gb_internal_palette = "GB - DMG"
        gambatte_gbc_color_correction = "GBC only"
        gambatte_gbc_color_correction_mode = "accurate"
        gambatte_gbc_frontlight_position = "central"
        gambatte_mix_frames = "accurate"
        gambatte_dark_filter_level = "0"
      '';
      shader = ''
        shaders = 1
        shader0 = ${shaderPath}/handheld/shaders/lcd3x.slang
        filter_linear0 = false
        scale_type0 = viewport
      '';
    };
    mgba = {
      displayName = "mGBA";
      options = ''
        mgba_interframe_blending = "mix"
      '';
      shader = ''
        shaders = 1
        shader0 = ${shaderPath}/handheld/shaders/lcd3x.slang
        filter_linear0 = false
        scale_type0 = viewport
      '';
    };
  };

  coreOptionFiles = lib.mapAttrs' (_: cfg:
    lib.nameValuePair "retroarch/config/${cfg.displayName}/${cfg.displayName}.opt" { text = cfg.options; }
  ) (lib.filterAttrs (_: cfg: cfg ? options) coreConfigs);

  coreShaderFiles = lib.mapAttrs' (_: cfg:
    lib.nameValuePair "retroarch/config/${cfg.displayName}/${cfg.displayName}.slangp" { text = cfg.shader; }
  ) (lib.filterAttrs (_: cfg: cfg ? shader) coreConfigs);
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
      video_shader_enable = "true";
      video_scale_integer = "true";  # Automatic integer scale
      aspect_ratio_index = "22";     # "Core Provided": let each core report its native ratio
      video_smooth = "false";        # Nearest-neighbor for sharp pixels

      # Prevent RetroArch from overwriting managed config
      config_save_on_exit = "false";
    };
  };

  xdg.configFile = coreOptionFiles // coreShaderFiles;

  home.packages = [
    pkgs.mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
    pkgs.maxcso      # To convert to CSO
  ];
}
