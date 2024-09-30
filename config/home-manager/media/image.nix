{ pkgs, lib, config, headless, hdr, ... }:
{
  home.packages = with pkgs; [
    exiftool
  ];

  programs.imv.enable = pkgs.stdenv.isLinux && !headless;
  home.shellAliases = lib.mkIf (pkgs.stdenv.isLinux && !headless) {
    "img" = "${lib.getExe pkgs.imv}";
    "webp_to_png" = ''nix-shell -p libwebp -p parallel --command "parallel dwebp {} -o {.}.png ::: *.webp"'';
  };
}
