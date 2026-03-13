{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.kavita = {
        enable = lib.mkEnableOption "Kavita permissions for this user";
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
