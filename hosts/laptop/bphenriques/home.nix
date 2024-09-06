{ pkgs, config, ... }:
{
  imports = [
    ../../../config/home-manager
    ../../../config/home-manager/desktop/firefox
    ../../../config/home-manager/desktop/logseq
    ../../../config/home-manager/desktop/discord.nix
    ../../../config/home-manager/dev/scala
    ../../../config/home-manager/misc/input-remapper
  ];

  home.sessionVariables.BROWSER = "firefox";
  programs.firefox.profiles.default.bookmarks = import ./git-secrets/bookmarks.sops.nix;
  # programs.beets.settings.directory = config.user.musicDir;

  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  custom = {
    lutris.enable = true;
    proton-run = {
      enable = true;
      defaultProtonDir = "/mnt/games/GlobalProton";
    };
    steam.enable = true;
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = [
    # Tighten permissions to private keys
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
    "z ${config.home.homeDirectory}/.gnupg  0700 ${config.home.username} users"

    # Tidy up most things under $HOME
    "L ${config.home.homeDirectory}/games                     - - - - /mnt/games"
    "L ${config.xdg.userDirs.documents}                       - - - - /mnt/bphenriques"
    "L ${config.xdg.userDirs.pictures}                        - - - - /mnt/nas-bphenriques/photos"
    "L ${config.xdg.userDirs.music}                           - - - - /mnt/nas-media/music"
    "d ${config.xdg.userDirs.desktop}                         - - - -"
    "d ${config.xdg.userDirs.download}                        - - - -"
    "d ${config.xdg.userDirs.extraConfig.XDG_SCREENSHOTS_DIR} - - - -"
  ];

  # TODO: should I enable https://github.com/NixOS/nixpkgs/issues/160923 ?
  # xdg.portal.enable = true;   # TODO: https://github.com/flatpak/xdg-desktop-portal. Should I set xdgOpenUsePortal?
  # https://github.com/bbigras/nix-config/blob/master/users/bbigras/graphical/mime.nix
  # https://github.com/ckiee/nixfiles/blob/3bcfddb139262222cd0e43a2a7fa08853a9e8697/modules/home/mimeapps.nix
  # https://github.com/Dragyx/nichts/blob/c2e0f26def0fd1ffe1e34b2491bc5f68393bc974/modules/other/xdg.nix#L94
  # xdg portal is enabled by default in plasma6: https://github.com/NixOS/nixpkgs/blob/8843893c9b842fcac17263a5700ee496e2cbee7f/nixos/modules/services/desktop-managers/plasma6.nix#L243
  xdg.mimeApps.enable = true;

  xdg.userDirs = {
    enable = true;
    createDirectories = false;

    documents = "${config.home.homeDirectory}/workdir";
    pictures  = "${config.home.homeDirectory}/pictures";
    music     = "${config.home.homeDirectory}/music/";
    desktop   = "${config.home.homeDirectory}/desktop";
    download  = "${config.home.homeDirectory}/downloads";

    extraConfig.XDG_SCREENSHOTS_DIR = "${config.home.homeDirectory}/screenshots"; # Non standard used by some apps.
  };

  home.stateVersion = "24.05";
}
