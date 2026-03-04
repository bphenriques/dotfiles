{ lib, config, ... }:
let
  cfg = config.custom.homelab.users;
  groupsCfg = config.custom.homelab.groups;

  syncthingDeviceOpt = lib.types.submodule {
    options = {
      name = lib.mkOption { type = lib.types.str; };
      id = lib.mkOption { type = lib.types.str; };
    };
  };

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
        pocket-id.enable = lib.mkEnableOption "Pocket-ID account for this user" // {
          enable = true;
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

        couchdb = {
          enable = lib.mkEnableOption "CouchDB account for this user";
          databases = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            readOnly = true;
            default = [ "obsidiandb-${name}" ];
          };
        };

        jellyfin = {
          enable = lib.mkEnableOption "Jellyfin account for this user";
          # FIXME: Remove once Jellyseerr supports OIDC - used for local Jellyfin auth
          passwordFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Path to file containing local Jellyfin password (for Jellyseerr auth until OIDC is supported)";
          };
        };

        jellyseerr = {
          enable = lib.mkEnableOption "Jellyseerr account for this user (requires Jellyfin)";
          permissions = {
            autoApprove = lib.mkEnableOption "auto-approve requests";
            advancedRequests = lib.mkEnableOption "advanced request options (e.g., quality profile)";
            viewRecentlyAdded = lib.mkEnableOption "view recently added media";
          };
        };

        wireguard = {
          enable = lib.mkEnableOption "Wireguard configuration for this user (requires Wireguard)";
          devices = lib.mkOption {
            type = lib.types.listOf (lib.types.submodule {
              options = {
                name = lib.mkOption { type = lib.types.str; description = "Device name (e.g., phone, laptop)"; };
                fullAccess = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = "If true, device can access entire network. If false, only the home server.";
                };
              };
            });
            default = [];
            description = "List of WireGuard devices for this user";
          };
        };

        syncthing = {
          enable = lib.mkEnableOption "Syncthing configuration for this user";
          devices = lib.mkOption {
            type = lib.types.listOf syncthingDeviceOpt;
            default = [ ];
          };
        };

        kavita = {
          enable = lib.mkEnableOption "Kavita permissions for this user";
          libraries = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ "Books" "Manga" "Comics" ];
            description = "Kavita libraries this user can access (derives kavita-library-<name> roles).";
            example = [ "Books" "Manga" "Comics" ];
          };
        };
      };
    };
  });
in
{
  options.custom.homelab = {
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
        couchdb = lib.filterAttrs (_: u: u.services.couchdb.enable) cfg;
        jellyfin = lib.filterAttrs (_: u: u.services.jellyfin.enable) cfg;
        jellyseerr = lib.filterAttrs (_: u: u.services.jellyseerr.enable) cfg;
        wireguard = lib.filterAttrs (_: u: u.services.wireguard.enable) cfg;
        syncthing = lib.filterAttrs (_: u: u.services.syncthing.enable) cfg;
        kavita = lib.filterAttrs (_: u: u.services.kavita.enable) cfg;
      };
      description = "Read-only attrset of users filtered by enabled service.";
    };
  };

  config.assertions = let
    jellyseerrUsers = lib.attrValues config.custom.homelab.enabledUsers.jellyseerr;
  in [
    {
      assertion = lib.all (u: u.services.jellyfin.enable) jellyseerrUsers;
      message = "All Jellyseerr users must have jellyfin.enable = true.";
    }
    {
      # FIXME: Remove once Jellyseerr supports OIDC
      assertion = lib.all (u: u.services.jellyfin.passwordFile != null) jellyseerrUsers;
      message = "All Jellyseerr users must have a Jellyfin passwordFile until Jellyseerr supports OIDC.";
    }
  ];
}
