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

  home.stateVersion = "24.05";
}

#  home = {
#    xdg.userDirs = {
#      enable = true;
#      createDirectories = false;  # Do not create any of the folders as they are being mounted.
#      documents = "${homeSambaServer.sharedFolder.personal.destination}/paperwork";
#      music = "${homeSambaServer.sharedFolder.media.destination}/music";
#      pictures = "${homeSambaServer.sharedFolder.personal.destination}/photos";
#      videos = "${homeSambaServer.sharedFolder.personal.destination}/videos";
#    };
#  };

# TODO: https://search.nixos.org/options?channel=unstable&show=networking.networkmanager.ensureProfiles.profiles&from=0&size=200&sort=relevance&type=packages&query=networking.networkmanager.ensureProfiles

