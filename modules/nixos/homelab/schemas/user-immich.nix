{ lib, ... }:
{
  options.services.immich.enable = lib.mkEnableOption "Immich account for this user";
}
