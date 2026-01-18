{ lib, config, ... }:
let
  cfg = config.custom.home-server.users;

  userOpt = lib.types.submodule ({ name, ... }: {
    options = {
      username = lib.mkOption { type = lib.types.str; default = name; };
      email = lib.mkOption { type = lib.types.str; description = "Email address for the user."; };
      name = lib.mkOption { type = lib.types.str; description = "Display name of the user."; };

      services = {
        pocket-id = {
          enable = lib.mkEnableOption "Pocket-ID account for this user";
          groups = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ "users" ];
            description = "Groups to assign in Pocket-ID. If 'admins' is included, the user is marked as admin.";
          };
        };

        immich.enable = lib.mkEnableOption "Immich account for this user";

        obsidian-livesync = {
          enable = lib.mkEnableOption "Obsidian LiveSync (CouchDB) account for this user";
          passwordFile = lib.mkOption { type = lib.types.path; };
          databases = lib.mkOption { type = lib.types.listOf lib.types.str; default = []; };
        };
      };
    };
  });
in
{
  options.custom.home-server.users = lib.mkOption {
    type = lib.types.attrsOf userOpt;
    default = { };
  };
}
