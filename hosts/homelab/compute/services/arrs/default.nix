_: {
  imports = [
    # Core
    ./sonarr.nix
    ./radarr.nix
    ./prowlarr
    ./jellyseerr

    # Quality of life
    ./cleanuparr.nix
    ./recyclarr.nix
  ];

  # TODO: https://gethomepage.dev/widgets/services/calendar/
}