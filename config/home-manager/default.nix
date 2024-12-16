{ pkgs, lib, config, ... }:
{
  imports = [
    ./coding
    ./internet
    ./media
    ./terminal
  ];

  # XDG Compliance to tidy up $HOME.
  xdg.enable = true;
  home.preferXdgDirectories = true;

  # Default apps and directories
  xdg.mimeApps.enable = pkgs.stdenv.isLinux;

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

    # Other core
    xdg-user-dirs

    # Archive
    p7zip     # 7zip for linux
    unrar     # Still need it
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    #     pkgs.nerd-fonts.hack
    #    pkgs.nerd-fonts.jetbrains-mono
    (nerdfonts.override { fonts = [ "Hack" "JetBrainsMono" ]; })
    baobab   # Visual disk space analyzer
  ];
  fonts.fontconfig.enable = true;

  # Gpg
  programs.gpg.enable = pkgs.stdenv.isLinux;
  services.gpg-agent = {
    enable = pkgs.stdenv.isLinux;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  home.sessionVariables = {
    LANG    = "en_US.UTF-8";
    LC_ALL  = "en_US.UTF-8";
  };

  # Do not need the whole home-manager configuration reference.
  manual.manpages.enable = false;

  # Tighten permissions to private keys
  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
    "z ${config.home.homeDirectory}/.gnupg  0700 ${config.home.username} users"
  ];
}
