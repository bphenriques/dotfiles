{ config, lib, pkgs, ... }:
let
  inherit (config.custom) shares;
  mounted = lib.filterAttrs (_: s: s.root != null) shares;
  backed = lib.filterAttrs (_: s: s.backup) mounted;
  skipped = lib.attrNames (lib.filterAttrs (_: s: !s.backup) mounted);
in
{
  selfhost.tasks.backup.integrations.notify.enable = true;

  custom.shares = {
    bphenriques.backup = true;
    media.backup = true;
  };

  # Opt-in is the safe default for a per-byte cost, but a share nobody decided about should not stay
  # quiet: `shared` was mounted and unbacked for a long time without anything saying so.
  warnings = lib.optional (
    skipped != [ ]
  ) "Shares mounted here but excluded from the off-site backup: ${toString skipped}. Set custom.shares.<name>.backup if unintended.";

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
      # App DBs (Immich/Miniflux/RomM) are deliberately not dumped — in a real disaster they are trivially
      # rebuilt (re-scan) or non-critical; the irreplaceable data (files, gitea repos, config) is covered here.
      services = [ "bazarr" "gitea" "home-assistant" "radarr" "radicale" "sonarr" ];
      # Whole shares, so a new folder is protected by default; exclusions live in each share's
      # .gitignore, which rustic honours via git-ignore.
      bindings =
        { "/system/homelab-secrets" = config.selfhost.runtimeSecretsDir; }
        // lib.mapAttrs' (name: s: lib.nameValuePair "/nas/${name}" s.root) backed;
    };
  };
}
