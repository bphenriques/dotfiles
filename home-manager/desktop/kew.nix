{ pkgs, lib, self, config, ... }:
{
  # Alternatives:
  # - cmus for local files but lacks artwork
  # - mpd (server) + rmpc (client) + mpd-mpris (mpris integration) + mpris-notifier (notifications). Too much...?
  home.packages = [
    pkgs.kew
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    (pkgs.makeDesktopItem {
      name = "kew";
      desktopName = "kew";
      icon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } "kew" "";
      exec = ''${lib.getExe' pkgs.foot "footclient"} --title=kew-tui ${lib.getExe pkgs.kew}'';
    })
  ];
}