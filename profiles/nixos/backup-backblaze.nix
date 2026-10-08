# Off-site backup to the shared Backblaze B2 repo. Compute and storage are co-tenants, so credentials
# and retention are one fact; what each feeds in, and which prunes, stays with the host.
{ config, ... }:
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

  selfhost.backup.targets.backblaze = {
    repository = "opendal:b2";
    backendCredentialsFile = config.sops.templates."homelab-backup-secrets.toml".path;
    passwordFile = config.sops.secrets."backup/rustic/password".path;
    retention = {
      daily = "7 days";
      weekly = "1 month";
      monthly = "1 year";
      yearly = "2 years";
    };
  };
}
