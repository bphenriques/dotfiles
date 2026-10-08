{ config, self, osConfig, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  nasShares = osConfig.fleet.shares;
in
{
  imports = [
    ../../../profiles/home-manager/base.nix
    ../../../profiles/home-manager/graphical
    ../../../profiles/home-manager/desktop
    ../../../profiles/home-manager/development
    ../../../profiles/home-manager/gaming
    ./kanshi.nix
  ];

  # NAS symlinks. Avoid mounting directly to $HOME to prevent slowdowns when offline
  systemd.user.tmpfiles.rules = [
    "L ${config.xdg.userDirs.pictures}/nas  - - - - ${nasShares.bphenriques.root}/photos"
    "L ${config.xdg.userDirs.music}/nas     - - - - ${nasShares.media.root}/music"
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
  ];

  gtk.gtk3.bookmarks = [
    "file://${nasShares.bphenriques.root} NAS Private"
    "file://${nasShares.media.root} NAS Media"
    "file://${nasShares.bphenriques.root}/documents NAS Documents"
    "file://${nasShares.media.root}/movies NAS Movies"
    "file://${nasShares.media.root}/tv NAS TV"
    "file://${nasShares.media.root}/downloads NAS Downloads"
  ];

  my.dotfiles.enable = true;
  my.programs.file-explorer = {
    enable = true;
    bookmarks = [
      {
        name = "NAS Private";
        icon = mkIcon "nas-private" "󰉐";
        path = nasShares.bphenriques.root;
      }
      {
        name = "NAS Media";
        icon = mkIcon "nas-media" "󰥠";
        path = nasShares.media.root;
      }
      {
        name = "NAS Documents";
        icon = mkIcon "nas-documents" "󰈙";
        path = "${nasShares.bphenriques.root}/documents";
      }
      {
        name = "NAS Movies";
        icon = mkIcon "nas-movies" "󰎁";
        path = "${nasShares.media.root}/movies";
      }
      {
        name = "NAS TV";
        icon = mkIcon "nas-tv" "󰟴";
        path = "${nasShares.media.root}/tv";
      }
      {
        name = "NAS Downloads";
        icon = mkIcon "nas-downloads" "󰇚";
        path = "${nasShares.media.root}/downloads";
      }
    ];
  };

  wayland.windowManager.niri.settings.input = {
    touchpad = { tap = { }; natural-scroll = { }; drag = false; };
    mouse.accel-profile = "flat";
  };

  home.stateVersion = "24.05";
}
