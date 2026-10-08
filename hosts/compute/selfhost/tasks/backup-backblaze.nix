# Repo, credentials and retention come from profiles/nixos/backup-backblaze.nix.
{ config, ... }:
{
  selfhost.backup.targets.backblaze = {
    # Storage prunes this shared repository; rustic takes no lock, and two concurrent prunes read packs
    # the other is deleting. Forget still runs here, and storage reclaims this host's space too.
    prune = false;
    # App DBs (Immich/Miniflux/RomM) are deliberately not dumped: in a real disaster they are trivially
    # rebuilt (re-scan) or non-critical; the irreplaceable data (files, gitea repos, config) is covered here.
    services = [ "bazarr" "gitea" "home-assistant" "radarr" "radicale" "sonarr" "wireguard" ];
    # The NAS shares are storage's job now: it reads the datasets locally and records their real
    # ownership, where this host's CIFS mounts could only record a fabricated uid/gid.
    bindings."/system/homelab-secrets" = config.selfhost.runtimeSecretsDir;
  };
}
