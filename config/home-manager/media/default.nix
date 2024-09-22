{ pkgs, lib, config, headless, ... }:
{
  home.packages = with pkgs; [
    exiftool
  ] ++ lib.optionals (pkgs.stdenv.isLinux && !headless) [
    museeks   # Audio
    vlc       # Video
    # TODO: Convert webp to png: parallel dwebp {} -o {.}.png ::: *.webp using libwebp
  ];
}
