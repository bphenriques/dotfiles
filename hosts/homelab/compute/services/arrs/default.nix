_: {
  imports = [
    # Schema
    ./media.nix

    # Core
    ./sonarr
    ./radarr
    ./prowlarr
    ./jellyseerr

    # Quality of life
    ./cleanuparr.nix
    ./recyclarr.nix
  ];
}
