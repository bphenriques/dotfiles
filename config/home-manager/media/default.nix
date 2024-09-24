{ pkgs, lib, config, headless, hdr, ... }:
{
  home.packages = with pkgs; [
    exiftool
  ] ++ lib.optionals (pkgs.stdenv.isLinux && !headless) [
    feishin   # Audio
    # TODO: Convert webp to png: parallel dwebp {} -o {.}.png ::: *.webp using libwebp
  ];

  programs.imv.enable = pkgs.stdenv.isLinux && !headless; # Image Viewer

  # Video Player: https://mpv.io/manual/master/
  programs.mpv = {
    enable = pkgs.stdenv.isLinux && !headless;
    config = {
      alang = "eng,en,enUS,en-US";        # Audio track
      slang = "eng,en,enUS,en-US,por,pt"; # Subtitle track

      fullscreen = true;

      # Tweaks to video rendering
      hwdec = "auto-safe";
      deband = true;
    };
    bindings = {
      "HOME" = "seek 0 absolute";

      # Dynamic Audio Normalizer: https://ffmpeg.org/ffmpeg-filters.html#dynaudnorm
      "CTRL+v" = "af toggle dynaudnorm=framelen=250:gausssize=11:maxgain=12:peak=0.8:targetrms=0.8";
    };
  };
}
