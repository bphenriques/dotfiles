{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    (_: {
      options.services.immich.enable = lib.mkEnableOption "Immich account for this user";
    })
  ];
}
