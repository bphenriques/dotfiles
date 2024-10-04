{ pkgs, lib, config, ... }:
{
  programs.zathura = {
    enable = pkgs.stdenv.isLinux;
    options = {
      guioptions = "v";
      adjust-open = "width";
      statusbar-basename = true;
      selection-clipboard = "clipboard";
      render-loading = false;
      scroll-step = 120;
    };
  };

  custom.xdgDefaultApps.document = lib.mkBefore [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
}
