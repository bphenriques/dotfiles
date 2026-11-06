# Repo, credentials and retention come from profiles/nixos/backup-backblaze.nix.
{ config, ... }:
{
  selfhost.backup.targets.backblaze = {
    # Storage prunes this shared repo (rustic has no lock); forget still runs here.
    prune = false;
    # App DBs are not dumped: rebuildable by re-scan or non-critical.
    services = [ "bazarr" "gitea" "home-assistant" "ntfy" "radarr" "radicale" "sonarr" "wireguard" ];
    # NAS shares are backed up by storage, which records real ownership instead of CIFS's fabricated ids.
    bindings."/system/homelab-secrets" = config.selfhost.runtimeSecretsDir;
  };
}
