{ pkgs, lib, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.zathura = {
    enable = true;
    options = {
      guioptions = "v";
      adjust-open = "width";
      selection-clipboard = "clipboard";
      render-loading = false;
      statusbar-basename = true;
      statusbar-home-tilde = true;    # Replace $HOME full path with '~'
      window-title-home-tilde = true; # Replace $HOME full path with '~'

      # Scroll
      scroll-step = 100;
      smooth-scroll = true;
      scroll-page-aware = "true";
      scroll-full-overlap = "0.01";
    };
  };

  custom.xdgDefaultApps.document = lib.mkBefore [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
}
