{ pkgs, config, ... }:
{
  imports = [
    ./home-impermanence.nix
    ../../../config/home-manager
    ../../../config/home-manager/desktop/plasma.nix
    ../../../config/home-manager/desktop/firefox
    ../../../config/home-manager/desktop/logseq
    ../../../config/home-manager/desktop/discord.nix
    ../../../config/home-manager/dev/scala
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
    # Set up working directory that is a ZFS dataset and move relevant data there
    "L /home/bphenriques/workdir  - - - - /mnt/bphenriques"

    # Set up relevant media directories and set in a way that XDG USER DIRECTORIES capture both local and the NAS files
    "L /home/bphenriques/pictures/    - - - -   /mnt/nas-bphenriques/photos"
    "L /home/bphenriques/paperwork/   - - - -   /mnt/nas-bphenriques/paperwork"
    "L /home/bphenriques/music/       - - - -   /mnt/nas-media/music"

    # Fix permissions to security
    "z ${config.custom.impermanence.dataLocation}/.ssh    0700 bphenriques users"
    "z ${config.custom.impermanence.dataLocation}/.gnupg  0700 bphenriques users"
  ];

  xdg.userDirs = {
    documents = "/home/bphenriques/workdir";
    music = "/home/bphenriques/music/";
    pictures = "/home/bphenriques/pictures";
  };

  home.stateVersion = "24.05";
}
