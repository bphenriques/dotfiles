{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.kavita = {
        enable = lib.mkEnableOption "Kavita permissions for this user";
        passwordFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to file containing Kavita password for local authentication";
        };
        libraries = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "Books" "Manga" "Comics" ];
          description = "Kavita libraries this user can access (derives kavita-library-<name> roles).";
          example = [ "Books" "Manga" "Comics" ];
        };
      };
    })
  ];
}
