{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    (_: {
      options.services.kavita = {
        enable = lib.mkEnableOption "Kavita permissions for this user";
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing Kavita password for local authentication";
        };
      };
    })
  ];
}
