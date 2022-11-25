{ ... }:
{
  username = "brunohenriques";
  hostDarwinModules = [
    ../../darwin/common.nix
    ../../darwin/work.nix
    { system.desktop.picture = ./wallpaper.png; } # From simpledesktops
  ];

  hostHomeManagerModules = [
    ../../home/common.nix
    ../../home/work.nix
  ];
}
