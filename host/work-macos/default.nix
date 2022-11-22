{ ... }:
{
  username = "brunohenriques";
  hostDarwinModules = [
    ../../macos/common.nix
    ../../macos/work.nix
    { system.desktop.picture = ./wallpaper.png; } # From simpledesktops
  ];

  hostHomeManagerModules = [
    ../../home/common.nix
    ../../home/work.nix
  ];
}
