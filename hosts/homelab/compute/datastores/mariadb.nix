# MariaDB Datastore
#
# Used by:
# - Romm
{ pkgs, ... }:
{
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };
}
