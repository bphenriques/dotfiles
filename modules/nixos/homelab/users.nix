{ lib, config, ... }:
let
  cfg = config.custom.homelab;

  baseUserModule = { name, config, ... }: {
    options = {
      username = lib.mkOption { type = lib.types.str; default = name; };
      email = lib.mkOption { type = lib.types.str; };
      firstName = lib.mkOption { type = lib.types.str; };
      lastName = lib.mkOption { type = lib.types.str; };
      name = lib.mkOption { type = lib.types.str; default = "${config.firstName} ${config.lastName}"; };
      groups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ cfg.groups.users ];
        description = "Groups assigned to this user. If admin group is included, the user is marked as admin.";
      };
      isAdmin = lib.mkOption { type = lib.types.bool; readOnly = true; default = lib.elem cfg.groups.admin config.groups; };
    };
  };
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

      guests = lib.mkOption {
        type = lib.types.str;
        default = "guests";
        description = "Name of the guests group";
      };
    };

    _userOptionExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.deferredModule;
      default = [ ];
      internal = true;
    };

    users = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submoduleWith {
        modules = [ baseUserModule ] ++ cfg._userOptionExtensions;
      });
      default = { };
    };
  };
}
