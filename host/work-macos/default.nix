{ ... }:
{
  username          = "brunohenriques";
  hostDarwinModules = [
    ../../macos/work.nix
    { system.desktop.picture = ./wallpaper.png; }
  ];
  hostHomeManagerModules = [../../home/work.nix];
}
