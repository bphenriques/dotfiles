{ ... }:
{
  username          = "brunohenriques";
  hostDarwinModules = [
    ../../macos/common.nix
    ../../macos/work.nix
    { system.desktop.picture = ./wallpaper.png; }
  ];
  
  hostHomeManagerModules = [
    ../../home/common.nix
    ../../home/work.nix
  ];
}
