{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.tandoor = {
        enable = lib.mkEnableOption "Tandoor account for this user";
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing Tandoor password for local authentication";
        };
        group = lib.mkOption {
          type = lib.types.enum [ "guest" "user" "admin" ];
          default = "user";
          description = "Tandoor permission group for this user";
        };
      };
    })
  ];
}
