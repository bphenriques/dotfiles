{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    (_: {
      options.services.syncthing = {
        enable = lib.mkEnableOption "Syncthing configuration for this user";
        devices = lib.mkOption {
          type = lib.types.listOf (lib.types.submodule {
            options = {
              name = lib.mkOption { type = lib.types.str; };
              id = lib.mkOption { type = lib.types.str; };
            };
          });
          default = [ ];
        };
      };
    })
  ];
}
