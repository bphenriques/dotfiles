{ lib, ... }:
{
  options.services.radicale.enable = lib.mkEnableOption "Radicale CalDAV/CardDAV access for this user";
}
