{ lib, pkgs, config, osConfig, ... }:
let
  emulationPaths = osConfig.custom.homelab.paths.media.gaming.emulation;
  stateDir = "${config.xdg.stateHome}/retroarch";

  # Per-core config: https://docs.libretro.com/library/
  # Option key names verified against libretro source code.
  shaderPath = "${pkgs.libretro-shaders-slang}/share/libretro/shaders/shaders_slang";

  toRetroArchConfig = lib.generators.toKeyValue {
    mkKeyValue = k: v: k + " = " + builtins.toJSON v;
  };

  # Shared shader presets — slang format requires video_driver = "vulkan" (or d3d)
  # Reference: https://retrogamecorps.com/2024/09/01/guide-shaders-and-overlays-on-retro-handhelds/
  lcdShader = ''
    shaders = 1
    shader0 = ${shaderPath}/handheld/shaders/lcd3x.slang
    filter_linear0 = false
    scale_type0 = viewport
  '';
  crtShader = ''
    shaders = 1
    shader0 = ${shaderPath}/crt/shaders/crt-gdv-mini-ultra.slang
    scale_type0 = viewport
  ''; # May need to lower Saturation Boost from ~1.20 to ~0.9 via Quick Menu > Shaders > Parameters
  crt3dShader = ''
    shaders = 4
    shader0 = ${shaderPath}/crt/shaders/newpixie/accumulate.slang
    alias0 = accum1
    scale0 = 1.0
    scale_type0 = source
    filter_linear0 = true
    shader1 = ${shaderPath}/crt/shaders/newpixie/blur_horiz.slang
    alias1 = blur1
    scale_1 = 1.0
    scale_type1 = source
    filter_linear1 = true
    shader2 = ${shaderPath}/crt/shaders/newpixie/blur_vert.slang
    alias2 = blur2
    scale_2 = 1.0
    scale_type2 = source
    filter_linear2 = true
    shader3 = ${shaderPath}/crt/shaders/newpixie/newpixie-crt.slang
    filter_linear3 = true
    scale_type3 = viewport
    textures = "frametexture"
    frametexture = ${shaderPath}/crt/shaders/newpixie/crtframe.png
    frametexture_linear = true
    frametexture_wrap_mode = clamp_to_border
    parameters = "use_frame;curvature;vignette;ghosting;wiggle_toggle;scanroll"
    use_frame = "0.0"
    curvature = "1.0"
    vignette = "0.3"
    ghosting = "1.0"
    wiggle_toggle = "0.0"
    scanroll = "1.0"
  ''; # Tweak curvature/vignette via Quick Menu > Shaders > Parameters
  # Key: canonical core id. `displayName` must match RetroArch's core display name exactly
  # (used for per-core config/shader directory names).
  coreConfigs = {
    genesis_plus_gx = {
      displayName = "Genesis Plus GX";
      options = {
        genesis_plus_gx_overscan = "disabled";
        genesis_plus_gx_render = "single field";
      };
      overrides = { rewind_enable = "true"; };
      shader = crtShader;
    };
    fceumm = {
      displayName = "FCEUmm";
      options = {
        fceumm_overscan_h = "enabled";
        fceumm_overscan_v = "enabled";
      };
      shader = crtShader;
    };
    snes9x = {
      displayName = "Snes9x";
      overrides = { rewind_enable = "true"; };
      shader = crtShader;
    };
    swanstation = {
      displayName = "SwanStation";
      options = {
        swanstation_GPU_ResolutionScale = "3";
        swanstation_GPU_TrueColor = "true";
        swanstation_GPU_TextureFilter = "Nearest";
        swanstation_GPU_PGXPEnable = "true";
        swanstation_GPU_PGXPCulling = "true";
        swanstation_GPU_PGXPTextureCorrection = "true";
      };
      overrides = {
        video_scale_integer = "false";
        aspect_ratio_index = "22";
      };
      shader = crt3dShader;
    };
    flycast = {
      displayName = "Flycast";
      options = {
        flycast_internal_resolution = "1280x960";
        flycast_anistropic_filtering = "2";
        flycast_enable_rtt = "On";
      };
      overrides = {
        video_scale_integer = "false";
        aspect_ratio_index = "22";
      }; # Do NOT enable rewind — causes corruption (https://github.com/flyinghead/flycast/issues/471)
    };
    desmume = {
      displayName = "DeSmuME";
      options = {
        desmume_internal_resolution = "512x384";
        desmume_screens_layout = "top/bottom";
        desmume_screens_gap = "0";
      };
      shader = lcdShader;
    };
    gambatte = {
      displayName = "Gambatte";
      options = {
        gambatte_gb_colorization = "internal";
        gambatte_gb_internal_palette = "GB - DMG";
        gambatte_gbc_color_correction = "GBC only";
        gambatte_gbc_color_correction_mode = "accurate";
        gambatte_gbc_frontlight_position = "central";
        gambatte_mix_frames = "accurate";
        gambatte_dark_filter_level = "0";
      };
      overrides = { rewind_enable = "true"; };
      shader = lcdShader;
    };
    mgba = {
      displayName = "mGBA";
      options = {
        mgba_interframe_blending = "mix";
      };
      overrides = { rewind_enable = "true"; };
      shader = lcdShader;
    };
    fbneo = {
      displayName = "FinalBurn Neo";
      shader = crtShader;
    };
    dosbox_pure = {
      displayName = "DOSBox-pure";
      shader = crtShader;
    };
    prboom = {
      displayName = "PrBoom";
      shader = crtShader;
    };
  };

  coreConfigFiles = lib.concatMapAttrs (_: { displayName, options ? {}, shader ? null, overrides ? {} }:
    let prefix = "retroarch/config/${displayName}/${displayName}";
    in lib.optionalAttrs (options != {})  { "${prefix}.opt".text = toRetroArchConfig options; }
    // lib.optionalAttrs (shader != null)  { "${prefix}.slangp".text = shader; }
    // lib.optionalAttrs (overrides != {}) { "${prefix}.cfg".text = toRetroArchConfig overrides; }
  ) coreConfigs;
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

      # Rewind: off globally, enabled per-core for lightweight systems
      rewind_enable = "false";

      # Gamepad hotkeys (Xbox-style button indices: Select=6, Start=7, LB=4, RB=5, X=2, Y=3, LT=axis+2, RT=axis+5)
      input_enable_hotkey_btn = "6";        # Select — hold to activate hotkeys
      input_exit_emulator_btn = "7";        # Select+Start — quit
      input_load_state_btn = "4";           # Select+LB — load state
      input_save_state_btn = "5";           # Select+RB — save state
      input_menu_toggle_btn = "3";          # Select+Y (North) — quick menu
      input_fps_toggle_btn = "2";           # Select+X (West) — toggle FPS
      input_rewind_axis = "+2";             # Select+LT — rewind
      input_hold_fast_forward_axis = "+5";  # Select+RT — fast forward
    };
  };

  xdg.configFile = coreConfigFiles;

  home.packages = [
    pkgs.mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
    pkgs.maxcso      # To convert to CSO
  ];
}
