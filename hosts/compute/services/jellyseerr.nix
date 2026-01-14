{ config, ... }:
{
  custom.home-server.services.jellyseer.port = 9099;
  services.jellyseerr = {
    enable = true;
    port = config.custom.home-server.services.jellyseer.port;
  };

  # TODO: consider API_KEY to automate some tasks
}


{ config, pkgs, lib, ... }:
let
  cfg = config.custom.home-server;
  # Internal URLs for the *arr stack
  sonarrUrl = "http://127.0.0.1:${toString cfg.services.sonarr.port}";
  radarrUrl = "http://127.0.0.1:${toString cfg.services.radarr.port}";
in
{
  config = lib.mkIf cfg.enable {

    # 1. Jellyseerr Service
    services.jellyseerr = {
      enable = true;
      openFirewall = true;
    };

    # 2. Register in Traefik (using your existing abstraction)
    custom.home-server.services.jellyseerr = {
      port = 5055;
      oidc.enable = true; # Protect it with Pocket ID if desired
    };

    # 3. The Automation Script
    systemd.services.jellyseerr-init = {
      description = "Configure Jellyseerr";
      after = [ "jellyseerr.service" "sonarr.service" "radarr.service" ];
      requires = [ "jellyseerr.service" ];
      wantedBy = [ "multi-user.target" ];

      path = [ pkgs.curl pkgs.jq ];

      script = ''
        API_URL="${config.custom.home-server.services.jellyseer.internalUrl}/api/v1"

        # Wait for Jellyseerr to be up
        until curl -s -f "$API_URL/status" > /dev/null; do
          echo "Waiting for Jellyseerr..."
          sleep 5
        done

        # Load Secrets
        JELLY_KEY=$(cat ${config.sops.secrets.jellyseerr_api_key.path})
        MEDIA_KEY=$(cat ${config.sops.secrets.media_api_key.path})

        # --- HELPER FUNCTIONS ---

        # Function to check if a service exists by name
        service_exists() {
          ENDPOINT=$1
          NAME=$2