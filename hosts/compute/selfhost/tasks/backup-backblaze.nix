{ config, pkgs, ... }:
{
  sops = {
    secrets."backup/b2/bucket" = { };
    secrets."backup/b2/bucket_id" = { };
    secrets."backup/b2/application_key_id" = { };
    secrets."backup/b2/application_key" = { };
    secrets."backup/rustic/password" = { };
    templates."homelab-backup-secrets.toml" = {
      owner = "root";
      group = "root";
      mode = "0400";
      content = ''
        [repository.options]
        bucket = "${config.sops.placeholder."backup/b2/bucket"}"
        bucket_id = "${config.sops.placeholder."backup/b2/bucket_id"}"
        application_key_id = "${config.sops.placeholder."backup/b2/application_key_id"}"
        application_key = "${config.sops.placeholder."backup/b2/application_key"}"
      '';
    };
  };

  selfhost.backup = {
    package = pkgs.selfhost.rustic-manage;
    targets.backblaze = {
      repository = "opendal:b2";
      backendCredentialsFile = config.sops.templates."homelab-backup-secrets.toml".path;
      passwordFile = config.sops.secrets."backup/rustic/password".path;
      retention = {
        daily = "7 days";
        weekly = "1 month";
        monthly = "1 year";
        yearly = "2 years";
      };
      # Storage prunes this shared repository; rustic takes no lock, and two concurrent prunes read packs
      # the other is deleting. Forget still runs here, and storage reclaims this host's space too.
      prune = false;
      # App DBs (Immich/Miniflux/RomM) are deliberately not dumped — in a real disaster they are trivially
      # rebuilt (re-scan) or non-critical; the irreplaceable data (files, gitea repos, config) is covered here.
      services = [ "bazarr" "gitea" "home-assistant" "radarr" "radicale" "sonarr" ];
      # The NAS shares are storage's job now: it reads the datasets locally and records their real
      # ownership, where this host's CIFS mounts could only record a fabricated uid/gid.
      bindings."/system/homelab-secrets" = config.selfhost.runtimeSecretsDir;
    };
  };
}
