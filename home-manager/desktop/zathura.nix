{ pkgs, lib, config, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  programs.zathura = {
    enable = true;
    options = {
      guioptions = "v";
      adjust-open = "width";
      selection-clipboard = "clipboard";
      render-loading = false;
      scroll-step = 120;

      statusbar-basename = true;
      statusbar-home-tilde = true;    # Replace $HOME full path with '~'
      window-title-home-tilde = true; # Replace $HOME full path with '~'
    };
  };

  custom.xdgDefaultApps.document = lib.mkBefore [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
}
