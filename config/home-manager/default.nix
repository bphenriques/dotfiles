{ pkgs, lib, config, ... }:
{
  imports = [
    ./coding
    ./internet
    ./media
    ./writing
    ./terminal
  ];

  xdg.enable = true;  # XDG Compliance to tidy up $HOME.
  home.preferXdgDirectories = true;

  home.packages = with pkgs; [
    # Consistent UNIX command line tools regardless of the OS
    coreutils
    findutils
    gnugrep
    watch
    tree
    parallel
    gnused
    dateutils
    unzip
    xclip

    # Utilitary
    p7zip     # 7zip for linux
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    (nerdfonts.override { fonts = [ "Hack" "JetBrainsMono" ]; })
  ] ++ lib.optionals (pkgs.stdenv.isLinux && config.custom.dotfiles.graphicalEnvironment) [
    baobab   # Visual disk space analyzer
  ];
  fonts.fontconfig.enable = true;

  home.sessionVariables = {
    LANG    = "en_US.UTF-8";
    LC_ALL  = "en_US.UTF-8";
  };
}
