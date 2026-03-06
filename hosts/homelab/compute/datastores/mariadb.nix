# MariaDB Datastore
#
# Used by:
# - Romm
#
# TODO: Add backup strategy (mysqldump to NAS)
{ pkgs, ... }:
{
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
  };
}
