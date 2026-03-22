# Servics: Miniflux, Immich
{ pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16; # Immich + pgvecto.rs requires <= 16
  };
}
