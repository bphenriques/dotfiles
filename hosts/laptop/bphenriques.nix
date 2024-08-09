{ pkgs, config, ... }:
{
  imports = [
    ../../home
    ../../home/plasma
    ../../home/firefox
    ../../home/games/lutris.nix
    ../../home/games/sunshine.nix
  ];

  programs.plasma.workspace.wallpaper = ./wallpaper.png;
  programs.firefox.profiles.default.bookmarks = import ./secrets/bookmarks.sops.nix;
  # programs.beets.settings.directory = config.user.musicDir;

  #     signal-desktop

  # Gpg
  programs.gpg.enable = true;
  services.gpg-agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  home.packages = with pkgs; [
    killall     # Useful
  ];

  # Gaming
  custom.proton-run.enable = true;
  custom.proton-run.defaultProtonDir = "/mnt/data/GlobalProton";

  xdg.userDirs.enable = true;
  xdg.userDirs.createDirectories = false;
  xdg.mimeApps.enable = true; # TODO: Create associations?

  # TODO: should I enable https://github.com/NixOS/nixpkgs/issues/160923 ?
  # xdg.portal.enable = true;   # TODO: https://github.com/flatpak/xdg-desktop-portal. Should I set xdgOpenUsePortal?

  # impermanence
  custom.impermanence = {
    enable = true;
    configLocation = "/persist/config/bphenriques";
    cacheLocation = "/persist/cache/bphenriques";
  };
  home.persistence = {
    "${config.custom.impermanence.configLocation}".directories = [
      ".config/vlc" # TODO: https://github.com/iynaix/dotfiles/blob/f0f8918caed8f4c245fa82fc505ae0de09a32f5c/home-manager/programs/vlc.nix#L10
      ".config/sops"

      # Gaming
      ".local/share/Steam"
      ".config/lutris"
      ".local/share/lutris"
   ];
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

