{ config, self, osConfig, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  nasPaths = osConfig.custom.paths;
  mounts = {
    nasPrivate = nasPaths.users.bphenriques.root;
    nasMedia = nasPaths.media.root;
  };
in
{
  imports = [
    ../../../profiles/home-manager/base.nix
    ../../../profiles/home-manager/capabilities/graphical
    ../../../profiles/home-manager/capabilities/desktop
    ../../../profiles/home-manager/capabilities/development
    ../../../profiles/home-manager/capabilities/gaming
    ./kanshi.nix
  ];

  # NAS symlinks. Avoid mounting directly to $HOME to prevent slowdowns when offline
  systemd.user.tmpfiles.rules = [
    "L ${config.xdg.userDirs.pictures}/nas  - - - - ${mounts.nasPrivate}/photos"
    "L ${config.xdg.userDirs.music}/nas     - - - - ${mounts.nasMedia}/music"
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"  # was in the shared HM base
  ];

  gtk.gtk3.bookmarks = [
    "file://${mounts.nasPrivate} NAS Private"
    "file://${mounts.nasMedia} NAS Media"
    "file://${nasPaths.users.bphenriques.notes} NAS Notes"
    "file://${nasPaths.users.bphenriques.documents.root} NAS Documents"
    "file://${nasPaths.media.movies} NAS Movies"
    "file://${nasPaths.media.tv} NAS TV"
    "file://${nasPaths.media.downloads.root} NAS Downloads"
  ];

  custom.dotfiles.enable = true;
  custom.programs.file-explorer = {
    enable = true;
    bookmarks = [
      {
        name = "NAS Private";
        icon = mkIcon "nas-private" "󰉐";
        path = mounts.nasPrivate;
      }
      {
        name = "NAS Media";
        icon = mkIcon "nas-media" "󰥠";
        path = mounts.nasMedia;
      }
      {
        name = "NAS Notes";
        icon = mkIcon "nas-notes" "󱞁";
        path = nasPaths.users.bphenriques.notes;
      }
      {
        name = "NAS Documents";
        icon = mkIcon "nas-documents" "󰈙";
        path = nasPaths.users.bphenriques.documents.root;
      }
      {
        name = "NAS Movies";
        icon = mkIcon "nas-movies" "󰎁";
        path = nasPaths.media.movies;
      }
      {
        name = "NAS TV";
        icon = mkIcon "nas-tv" "󰟴";
        path = nasPaths.media.tv;
      }
      {
        name = "NAS Downloads";
        icon = mkIcon "nas-downloads" "󰇚";
        path = nasPaths.media.downloads.root;
      }
    ];
  };

  custom.programs.niri.input = {
    touchpad = [ "tap" "natural-scroll" "drag false" ];
    mouse = [ ''accel-profile "flat"'' ];
  };

  home.stateVersion = "24.05";
}
