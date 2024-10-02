{ pkgs, lib, config, ... }:
{
  home.packages = with pkgs; [
    exiftool
  ];

  programs.imv.enable = pkgs.stdenv.isLinux;
  home.shellAliases = lib.mkIf (pkgs.stdenv.isLinux) {
    "webp_to_png" = ''nix-shell -p libwebp -p parallel --command "parallel dwebp {} -o {.}.png ::: *.webp"'';
  };

  custom.xdgDefaultApps.image = lib.mkBefore [ "imv.desktop" ];
}
