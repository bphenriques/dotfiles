# Repo, credentials and retention come from profiles/nixos/backup-backblaze.nix. This host prunes it.
{
  config,
  lib,
  private,
  ...
}:
let
  inherit (config.fleet) shares;
  mounted = lib.filterAttrs (_: s: s.root != null) shares;
  backed = lib.filterAttrs (_: s: s.backup) mounted;
  skipped = lib.attrNames (lib.filterAttrs (_: s: !s.backup) mounted);
in
{
  # The opt-in silently omits, so the exclusions have to stay visible.
  warnings = lib.optional (
    skipped != [ ]
  ) "Shares served here but excluded from the off-site backup: ${toString skipped}. Set my.storage.shares.<name>.backup if unintended.";

  sops.secrets."notify/backup-token" = { };

  # Publishes to compute's ntfy. No provider runs here to mint a publisher token, so it comes from this
  # host's own secrets; the README carries the one-time mint.
  selfhost.notify.url = private.settings.notify.url;
  selfhost.tasks.backup.integrations.notify.tokenFile = config.sops.secrets."notify/backup-token".path;

  # Resolve it on the LAN: the public record needs internet, and it is the only name this host looks up.
  networking.hosts.${config.fleet.lan.hosts.compute} = [ (lib.removePrefix "https://" private.settings.notify.url) ];

  # Local folders which will enable storing the ownership information making restores safer.
  selfhost.backup.targets.backblaze.bindings =
    lib.mapAttrs' (name: s: lib.nameValuePair "/nas/${name}" s.root) backed;
}
