{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.immich.enable = lib.mkEnableOption "Immich account for this user";
    })
  ];
}
