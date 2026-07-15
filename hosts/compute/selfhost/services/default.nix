{ config, private, ... }:
let
  inherit (config.custom.fleet.lan) hosts;
in
{
  imports = [
    ./radarr.nix
    ./sonarr.nix
    ./immich
    ./jellyfin
    ./kapowarr.nix
    ./kavita
    ./seerr
    ./prowlarr
    ./romm
    ./homepage
    ./syncthing.nix
    ./transmission.nix
    ./wireguard.nix
    ./home-assistant.nix
    ./cook-recipes.nix
    ./filebrowser.nix
    ./papra.nix
    ./mympd.nix
  ];

  selfhost.external = {
    synology-dsm = {
      displayName = "Synology";
      description = "NAS";
      url = "http://${hosts.bruno-home-nas}:5000";
      integrations.homepage.group = "Admin";
    };
    inky = {
      displayName = "Inky";
      description = "E-Ink Display";
      url = "http://${hosts.inky}";
      integrations.homepage.group = "Admin";
    };
    jetkvm = {
      displayName = "JetKVM";
      description = "Remote KVM";
      url = "http://${hosts.jetkvm}";
      integrations.homepage.group = "Admin";
    };
    share = {
      displayName = "Share";
      description = "Public file sharing (share-vm)";
      url = "https://share.${private.settings.domain}";
      integrations.homepage.group = "Admin";
    };
  };
}
