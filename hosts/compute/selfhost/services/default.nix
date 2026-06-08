{ config, private, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
in
{
  imports = [
    ./radarr
    ./sonarr
    ./immich
    ./jellyfin
    ./kapowarr.nix
    ./kavita
    ./miniflux
    ./seerr
    ./prowlarr
    ./romm
    ./homepage
    ./syncthing.nix
    ./transmission.nix
    ./wireguard
    ./home-assistant.nix
    ./radicale
    ./cook-recipes
    ./filebrowser
    ./gitea
    ./papra.nix
    ./bentopdf.nix
    ./mympd.nix
  ];

  selfhost.external = {
    synology-dsm = {
      displayName = "Synology";
      description = "NAS";
      url = "http://${hosts.bruno-home-nas}:5000";
      integrations.homepage.tab = "Admin";
    };
    inky = {
      displayName = "Inky";
      description = "E-Ink Display";
      url = "http://${hosts.inky}";
      integrations.homepage.tab = "Admin";
    };
    jetkvm = {
      displayName = "JetKVM";
      description = "Remote KVM";
      url = "http://${hosts.jetkvm}";
      integrations.homepage.tab = "Admin";
    };
  };
}
