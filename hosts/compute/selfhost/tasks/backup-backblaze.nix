{ config, pkgs, ... }:
let
  inherit (config.selfhost) paths;
in
{
  selfhost.tasks.backup.integrations.notify.enable = true;

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
      services = [ "gitea" "home-assistant" "miniflux" "radarr" "radicale" "sonarr" ];
      bindings = {
        "/system/homelab-secrets"               = config.selfhost.runtimeSecretsDir;
        "/nas/bphenriques/backups"              = paths.users.bphenriques.backups.root;
        "/nas/bphenriques/notes"                = paths.users.bphenriques.notes;
        "/nas/bphenriques/private"              = paths.users.bphenriques.private;
        "/nas/bphenriques/documents"            = paths.users.bphenriques.documents.root;
        "/nas/bphenriques/photos/library"       = paths.users.bphenriques.photos.library;
        "/nas/bphenriques/photos/inbox"         = paths.users.bphenriques.photos.inbox;
        "/nas/media/music/library"              = paths.media.music.library;
        "/nas/media/music/playlists"            = paths.media.music.playlists;
        "/nas/media/gaming/emulation/roms"      = paths.media.gaming.emulation.roms;
        "/nas/media/gaming/emulation/bios"      = paths.media.gaming.emulation.bios;
      };
    };
  };
}
