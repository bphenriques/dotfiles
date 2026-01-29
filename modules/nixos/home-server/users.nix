{ lib, config, ... }:
let
  cfg = config.custom.home-server.users;

  userOpt = lib.types.submodule ({ name, config, ... }: {
    options = {
      username = lib.mkOption { type = lib.types.str; default = name; };
      email = lib.mkOption { type = lib.types.str; };
      firstName = lib.mkOption { type = lib.types.str; };
      lastName = lib.mkOption { type = lib.types.str; };
      name = lib.mkOption { type = lib.types.str; default = "${config.firstName} ${config.lastName}"; };

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

        miniflux = {
          enable = lib.mkEnableOption "Miniflux settings for this user";
          settings = lib.mkOption {
            type = lib.types.attrs;
            default = { };
            description = ''
              Miniflux user settings applied via API (theme, display_mode, stylesheet, etc).
              WARNING: Do not put secrets here - this data is stored in the world-readable Nix store.
              See https://miniflux.app/docs/api.html#update-user for available fields.
            '';
            example = { theme = "dark_serif"; display_mode = "fullscreen"; };
          };
        };

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
