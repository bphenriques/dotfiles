{ pkgs, config, ... }:
{
  imports = [
    ../../../config/home-manager
    ../../../config/home-manager/desktop/plasma.nix
    ../../../config/home-manager/desktop/firefox
    ../../../config/home-manager/desktop/logseq
    ../../../config/home-manager/desktop/discord.nix
    ../../../config/home-manager/dev/scala
    ../../../config/home-manager/misc/input-remapper
  ];

  programs.plasma.workspace.wallpaper = ./git-secrets/desktop.sops.jpg;
  programs.firefox.profiles.default.bookmarks = import ./git-secrets/bookmarks.sops.nix;
  # programs.beets.settings.directory = config.user.musicDir;

  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  xdg.userDirs.enable = true;
  xdg.userDirs.createDirectories = false;
  xdg.mimeApps.enable = true; # TODO: Create associations?

  # TODO: should I enable https://github.com/NixOS/nixpkgs/issues/160923 ?
  # xdg.portal.enable = true;   # TODO: https://github.com/flatpak/xdg-desktop-portal. Should I set xdgOpenUsePortal?

  custom = {
    lutris.enable = true;
    sunshine = {
      enable = true;
      steamBigPicture = true;
    };
    proton-run = {
      enable = true;
      defaultProtonDir = "/mnt/games/GlobalProton";
    };
  };

  # https://www.mankier.com/5/tmpfiles.d
  systemd.user.tmpfiles.rules = [
    # Tidy up most things under $HOME for easier access
    "L ${config.xdg.userDirs.documents}           - - - - /mnt/bphenriques"
    "L ${config.xdg.userDirs.pictures}            - - - - /mnt/nas-bphenriques/photos"
    "L ${config.xdg.userDirs.music}               - - - - /mnt/nas-media/music"
    "L ${config.home.homeDirectory}/paperwork/    - - - - /mnt/nas-bphenriques/paperwork"

    # Tighten permissions to private keys
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
    "z ${config.home.homeDirectory}/.gnupg  0700 ${config.home.username} users"
  ];

  xdg.userDirs = {
    documents = "${config.home.homeDirectory}/workdir";
    pictures  = "${config.home.homeDirectory}/pictures";
    music     = "${config.home.homeDirectory}/music/";
    desktop   = "${config.home.homeDirectory}/desktop";
    download  = "${config.home.homeDirectory}/downloads";
  };

  home.persistence."${config.custom.impermanence.dataLocation}".directories = [
    { directory = ".dotfiles"; method = "symlink"; } # Testing something...
    "desktop"
    "downloads"
  ];
  custom.impermanence = {
    enable = true;
    dataLocation = "/persist/data/bphenriques";
    cacheLocation = "/persist/cache/bphenriques";

    # Enable for those who don't have a programs.<prog>.enable
    heroic = true;
    steam = true;
    gog = true;
    nvidia = true;
    protontricks = true;
    vlc = true;
    qbittorrent = true;
    discord = true;
    bitwarden = true;
    jetbrains = true;
    g4music = true;
    mesa = true;
    wine = true;
    winetricks = true;
    solaar = true;
    filezilla = true;

    # Scala
    scalacli = true;
    metals = true;
    coursier = true;
  };

  home.stateVersion = "24.05";
}
