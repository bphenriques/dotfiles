{ config, ... }:
let
  inherit (config.fleet.lan) hosts;
in
{
  imports = [
    ./dnsmasq.nix
    ./radarr.nix
    ./sonarr.nix
    ./bazarr.nix
    ./cleanuparr
    ./immich.nix
    ./jellyfin.nix
    ./kapowarr.nix
    ./kavita
    ./navidrome.nix
    ./seerr
    ./prowlarr
    ./romm.nix
    ./romm-5.3.1.nix
    ./homepage
    ./syncthing.nix
    ./transmission.nix
    ./wireguard.nix
    ./home-assistant.nix
    ./cook-recipes.nix
    ./couchdb.nix
    ./livesync-cli
    ./filebrowser.nix
    ./papra.nix
    ./open-webui
  ];

  selfhost.external = {
    inky = {
      displayName = "Inky";
      meta.description = "E-Ink Display";
      url = "http://${hosts.inky}";
      integrations.homepage.group = "Admin";
    };
    jetkvm = {
      displayName = "JetKVM";
      meta.description = "Remote KVM";
      url = "http://${hosts.jetkvm}";
      integrations.homepage.group = "Admin";
    };
  };
}
