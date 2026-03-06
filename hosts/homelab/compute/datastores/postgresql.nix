# Shared PostgreSQL instance used by:
# - Miniflux (auto-configured via createDatabaseLocally)
# - Immich (auto-configured via NixOS module, requires pgvecto.rs → max PG 16)
#
# TODO: Add backup strategy (pg_dump to NAS)
{ pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16; # Immich + pgvecto.rs requires <= 16
  };
}
