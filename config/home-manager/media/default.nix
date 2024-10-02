{ lib, ... }: {
  imports = [
    ./notes
    ./music.nix
    ./image.nix
    ./video.nix
    ./documents.nix
  ];

  custom.xdgDefaultApps.archive = lib.mkBefore [ "org.kde.ark.desktop" ];
  custom.xdgDefaultApps.fileBrowser = lib.mkBefore [ "org.kde.dolphin.desktop" "org.gnome.baobab.desktop" "yazi.desktop" ];
}
