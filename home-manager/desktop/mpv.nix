{ pkgs, lib, config, ... }:
# TODO: Check https://github.com/iynaix/dotfiles/blob/f0f8918caed8f4c245fa82fc505ae0de09a32f5c/home-manager/programs/mpv.nix
# TODO: https://github.com/diniamo/niqs/blob/53288d72902365ee8d3bfdd6aff0ec79eb7c1c36/home/mpv/anime.nix
# https://github.com/Samillion/ModernZ
let
  # Interesting guides:
  # - https://kokomins.wordpress.com/2019/10/14/mpv-config-guide/
  # - https://iamscum.wordpress.com/guides/videoplayback-guide/mpv-conf/
  #
  # To consider:
  # - custom profile for battery-friendly (this laptop is plugged most times, if not all the time).
  # - pkgs.mpvScripts.video-cutter: Requires ffmpeg. https://github.com/familyfriendlymikey/mpv-cut
in
{
  # Video Player: https://mpv.io/manual/master/
  programs.mpv = {
    enable = pkgs.stdenv.isLinux;
    config = {
      # UI
      fullscreen = true;
      keep-open = "always";           # Prevents auto-close upon playback complete.
      osd-duration = 500;             # Hide On-Screen-Display quickly.
      save-position-on-quit = "yes";  # Remember where I left.
      autofit = "50%";                # Start with 50% of the screen.

      # Using a custom OSC
      osc = false;
      osd-bar = false;

      # Audio
      alang = "eng,en,enUS,en-US";    # Preferred language for audio.

      # Screenshot:
      screenshot-format = "png";
      screenshot-high-bit-depth = "yes";    # Same output bitdepth as the video
      screenshot-png-compression = "2";     # Not to high, not too low.
      screenshot-directory = "${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR}";
      screenshot-template = "%f-%wH.%wM.%wS.%wT-#%#00n";

      # Subtitles
      slang = "eng,en,enUS,en-US,por,pt";   # Preferred langauge for subtitles
      demuxer-mkv-subtitle-preroll = "yes"; # Try to force subtitles to show while seeking
      subs-with-matching-audio = "yes";     # Makes it easier for me to follow what is happening.
      sub-fix-timing = "yes";               # Remove minor gaps or overlaps between subtitles
      sub-auto = "fuzzy";                   # Almost exact match for subtitles.
      sub-gauss = 1.0;                      # Improve image subtitles quality (blur).
      sub-gray = "yes";                     # Improve image subtitles quality (convert to grayscale).

      # Tweaks to video rendering
      profile = "gpu-hq";
      vo = "gpu-next";  # See https://github.com/mpv-player/mpv/wiki/GPU-Next-vs-GPU
      gpu-api = "vulkan";
      hwdec = "auto-safe";
    };
    bindings = {
      "HOME" = "seek 0 absolute";

      # Dynamic Audio Normalizer: https://ffmpeg.org/ffmpeg-filters.html#dynaudnorm
      "CTRL+v" = "af toggle dynaudnorm=framelen=250:gausssize=11:maxgain=12:peak=0.8:targetrms=0.8";

      # Audio delay and subtitle delay. 0.042s is 1 frame for a 24fps video
      "CTRL+=" = "add audio-delay 0.1";
      "CTRL+-" = "add audio-delay -0.1";
      "CTRL+." = "add sub-delay +0.042";
      "CTRL+," = "add sub-delay -0.042";

      # For the custom menu
      "mbtn_right" = "script-binding uosc/menu";
      "menu"       = "script-binding uosc/menu";
    };

    scripts = [
      pkgs.mpvScripts.uosc          # Custom UI with subtitles downloaded bundled in
      pkgs.mpvScripts.thumbfast     # Generate thumbnails while seeking
      pkgs.mpvScripts.dynamic-crop  # Delete hard-coded blackbars on-the-fly. Use SHIFT+C.
      pkgs.mpvScripts.vr-reversal   # Play 360 video. See https://github.com/dfaker/VR-reversal
    ];

    scriptOpts = {
      # https://github.com/tomasklaen/uosc
      uosc = {
        timeline_size = 25;
        timeline_persistency = "paused,audio";
        progress = "always";
        progress_size = 4;
        progress_line_width = 4;
        controls = "subtitles,<has_many_audio>audio,<has_many_video>video,<has_many_edition>editions,<stream>stream-quality";
        refine = "text_width";
        top_bar = "never";
      };
      thumbfast = {
        spawn_first = true;
        network = true;
        hwdec = true;
      };
    };
  };

  home.shellAliases = lib.optionalAttrs (pkgs.stdenv.isLinux) {
    "mpv360" = "${lib.getExe config.programs.mpv.package} --script-opts=360plugin-enabled=yes";
  };

  custom.xdgDefaultApps = {
    video = lib.mkBefore [ "mpv.desktop" ];
    audio = lib.mkBefore [ "mpv.desktop" ];
  };
}
