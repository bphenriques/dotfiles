{ lib, config, ... }:
let
  cfg = config.custom.home-server.users;
  groupsCfg = config.custom.home-server.groups;

  userOpt = lib.types.submodule ({ name, config, ... }: {
    options = {
      username = lib.mkOption { type = lib.types.str; default = name; };
      email = lib.mkOption { type = lib.types.str; };
      firstName = lib.mkOption { type = lib.types.str; };
      lastName = lib.mkOption { type = lib.types.str; };
      name = lib.mkOption { type = lib.types.str; default = "${config.firstName} ${config.lastName}"; };
      groups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ groupsCfg.users ];
        description = "Groups assigned to this user. If admin group is included, the user is marked as admin.";
      };
      isAdmin = lib.mkOption { type = lib.types.bool; readOnly = true; default = builtins.elem groupsCfg.admin config.groups; };

      services = {
        pocket-id.enable = lib.mkEnableOption "Pocket-ID account for this user";

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
          databases = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            readOnly = true;
            default = [ "obsidiandb-${name}" ];
          };
        };

        jellyfin.enable = lib.mkEnableOption "Jellyfin account for this user";
      };
    };
  });
in
{
  options.custom.home-server = {
    groups = {
      admin = lib.mkOption {
        type = lib.types.str;
        default = "admin";
        description = "Name of the admin group";
      };

      users = lib.mkOption {
        type = lib.types.str;
        default = "users";
        description = "Name of the users group";
      };

      allowed = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        readOnly = true;
        default = [ groupsCfg.admin groupsCfg.users ];
        description = "List of allowed group names (read-only, derived from admin and users)";
      };
    };

    users = lib.mkOption {
      type = lib.types.attrsOf userOpt;
      default = { };
    };

    enabledUsers = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      default = {
        pocket-id = lib.filterAttrs (_: u: u.services.pocket-id.enable) cfg;
        immich = lib.filterAttrs (_: u: u.services.immich.enable) cfg;
        miniflux = lib.filterAttrs (_: u: u.services.miniflux.enable) cfg;
        obsidian-livesync = lib.filterAttrs (_: u: u.services.obsidian-livesync.enable) cfg;
        jellyfin = lib.filterAttrs (_: u: u.services.jellyfin.enable) cfg;
      };
      description = "Read-only attrset of users filtered by enabled service.";
    };
  };
}
