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
    ../../../profiles/home-manager/music
    ./display.nix
  ];

  # NAS symlinks. Avoid mounting directly to $HOME to prevent slowdowns when offline
  systemd.user.tmpfiles.rules = [
    "L ${config.xdg.userDirs.pictures}/nas  - - - - ${nasShares.bphenriques.root}/photos"
    "L ${config.xdg.userDirs.music}/nas     - - - - ${nasShares.media.root}/music"
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
  ];

  gtk.gtk3.bookmarks = map (b: "file://${b.path} ${b.name}") config.my.programs.file-explorer.bookmarks;

  my.programs.file-explorer = {
    enable = true;
    bookmarks = [
      {
        name = "NAS Private";
        icon = mkIcon "nas-private" self.lib.glyphs."md-folder_lock";
        path = nasShares.bphenriques.root;
      }
      {
        name = "NAS Media";
        icon = mkIcon "nas-media" self.lib.glyphs."md-disc_player";
        path = nasShares.media.root;
      }
      {
        name = "NAS Documents";
        icon = mkIcon "nas-documents" self.lib.glyphs."md-file_document";
        path = "${nasShares.bphenriques.root}/documents";
      }
      {
        name = "NAS Movies";
        icon = mkIcon "nas-movies" self.lib.glyphs."md-movie";
        path = "${nasShares.media.root}/movies";
      }
      {
        name = "NAS TV";
        icon = mkIcon "nas-tv" self.lib.glyphs."md-television_classic";
        path = "${nasShares.media.root}/tv";
      }
      {
        name = "NAS Downloads";
        icon = mkIcon "nas-downloads" self.lib.glyphs."md-download";
        path = "${nasShares.media.root}/downloads";
      }
    ];
  };

  my.programs.volume-osd.pins = {
    sinks = [
      { match = "alsa_output.pci-*analog-stereo"; label = "Speakers"; type = "internal"; }
      { match = "*Arctis*"; label = "Headset"; type = "headset"; }
      { match = "*EDIFIER*"; label = "Desk speakers"; type = "external"; }
    ];
    sources = [
      { match = "alsa_input.pci-*"; label = "Microphone"; type = "microphone"; }
      { match = "*Arctis*"; label = "Headset"; type = "headset"; }
    ];
  };

  wayland.windowManager.niri.settings.input = {
    touchpad = { tap = { }; natural-scroll = { }; drag = false; };
    mouse.accel-profile = "flat";
  };

  home.stateVersion = "24.05";
}
