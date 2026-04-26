{ config, self, osConfig, ... }:
let
  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  mounts = {
    nasPrivate = osConfig.custom.homelab.paths.users.bphenriques.root;
    nasMedia = osConfig.custom.homelab.paths.media.root;
  };
in
{
  imports = [
    ../../../profiles/home-manager
    ../../../profiles/home-manager/desktop-environment
    ../../../profiles/home-manager/desktop
    ../../../profiles/home-manager/development
    ../../../profiles/home-manager/gaming
    ./kanshi.nix
  ];

  # NAS symlinks — avoid mounting directly to $HOME to prevent slowdowns when offline
  systemd.user.tmpfiles.rules = [
    "L ${config.xdg.userDirs.pictures}/nas  - - - - ${mounts.nasPrivate}/photos"
    "L ${config.xdg.userDirs.music}/nas     - - - - ${mounts.nasMedia}/music"
  ];

  gtk.gtk3.bookmarks = [
    "file://${mounts.nasPrivate}"
    "file://${mounts.nasMedia}"
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
    ];
  };

  custom.programs.niri.input = {
    touchpad = [ "tap" "natural-scroll" "drag false" ];
    mouse = [ ''accel-profile "flat"'' ];
  };

  home.stateVersion = "24.05";
}
