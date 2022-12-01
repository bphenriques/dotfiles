{ ... }:
{
  username = "brunohenriques";
  hostDarwinModules = [
    ../../darwin
    ./darwin.nix
    { system.desktop.picture = ./wallpaper.png; } # From simpledesktops
  ];

  hostHomeManagerModules = [
    ../../home
    ./home.nix
  ];
}
