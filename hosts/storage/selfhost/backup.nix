{
  config,
  lib,
  pkgs,
  private,
  ...
}:
let
  inherit (config.custom) shares;
  mounted = lib.filterAttrs (_: s: s.root != null) shares;
  backed = lib.filterAttrs (_: s: s.backup) mounted;
  skipped = lib.attrNames (lib.filterAttrs (_: s: !s.backup) mounted);
in
{
  # Household shares only; personal ones opt in from the private settings, so their names stay there.
  custom.storage.shares = {
    media.backup = true;
    shared.backup = true;
  };

  # Opt-in is the safe default for a per-byte cost, but exclusions must remain visible.
  warnings = lib.optional (
    skipped != [ ]
  ) "Shares served here but excluded from the off-site backup: ${toString skipped}. Set custom.storage.shares.<name>.backup if unintended.";

  # Manual restores and `rustic tag` run against the same generated profile as the timer.
  environment.systemPackages = [ pkgs.rustic ];

  sops = {
    secrets."backup/b2/bucket" = { };
    secrets."backup/b2/bucket_id" = { };
    secrets."backup/b2/application_key_id" = { };
    secrets."backup/b2/application_key" = { };
    secrets."backup/rustic/password" = { };
    secrets."notify/backup-token" = { };
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

  # Publishes to compute's ntfy. No provider runs here to mint a publisher token, so it comes from this
  # host's own secrets; the README carries the one-time mint.
  selfhost.notify.url = private.settings.notify.url;
  selfhost.tasks.backup.integrations.notify.tokenFile = config.sops.secrets."notify/backup-token".path;

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
    # Reading the datasets locally records their real ownership, unlike the CIFS mount on compute which
    # fabricated uid/gid and forced restores through --no-ownership.
    bindings = lib.mapAttrs' (name: s: lib.nameValuePair "/nas/${name}" s.root) backed;
  };
}
