{ lib, ... }:
{
  # Per-user config for consumer-owned concerns that are NOT part of selfhost-nix (our own services and
  # enrichments to selfhost-nix apps). Kept off `selfhost.users` so the framework schema stays clean;
  # our configures read this and join with `selfhost.users` by name. Mirrors the selfhost-nix path:
  #   custom.users.<name>.services.<app>   — a service of ours (jellyfin, immich, …)
  #   custom.users.<name>.selfhost.<concern> — enrichment to a selfhost-nix concern (e.g. apps.miniflux)
  options.custom.users = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        # Identity (username/email/isAdmin) and enrichments (selfhost.*) ride freeform; each consumer
        # module adds its typed `services.<app>` fragment here, so defaults apply like a real schema.
        freeformType = lib.types.attrsOf lib.types.anything;
      }
    );
    default = { };
    description = "Consumer-owned per-user config, keyed by username (joined with selfhost.users by our configures).";
  };
}
