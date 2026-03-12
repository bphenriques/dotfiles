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

    # Backup hooks
    ./sonarr-backup.nix
    ./radarr-backup.nix
  ];
}
