{ pkgs, config, ... }:
{
  imports = [
    ../../../config/home-manager
    ../../../config/home-manager/desktop/plasma.nix
    ../../../config/home-manager/media/firefox
    ../../../config/home-manager/media/logseq
    ../../../config/home-manager/gaming/lutris.nix
    ../../../config/home-manager/gaming/sunshine.nix
    ../../../config/home-manager/dev/scala
  ];

  programs.plasma.workspace.wallpaper = ./secrets/wallpaper.sops.jpg;
  programs.firefox.profiles.default.bookmarks = import ./secrets/bookmarks.sops.nix;
  # programs.beets.settings.directory = config.user.musicDir;

  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  # Gaming
  custom.proton-run.enable = true;
  custom.proton-run.defaultProtonDir = "/mnt/data/GlobalProton";

  xdg.userDirs.enable = true;
  xdg.userDirs.createDirectories = false;
  xdg.mimeApps.enable = true; # TODO: Create associations?

  # TODO: should I enable https://github.com/NixOS/nixpkgs/issues/160923 ?
  # xdg.portal.enable = true;   # TODO: https://github.com/flatpak/xdg-desktop-portal. Should I set xdgOpenUsePortal?

  custom.impermanence = {
    enable = true;
    dataLocation = "/persist/data/bphenriques";
    cacheLocation = "/persist/cache/bphenriques";
  };
  home.persistence = {
    "${config.custom.impermanence.dataLocation}" = {
      directories = [
        ".config/vlc"

        # Gaming
        ".local/share/Steam"
        ".config/lutris"
        ".local/share/lutris"
     ];
     allowOther = false;
   };
   "${config.custom.impermanence.cacheLocation}".allowOther = false;
 };
  home.sessionVariables.SOPS_AGE_KEY_FILE = "/persist/data/system/var/lib/sops-nix/bphenriques-keys.txt";

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

