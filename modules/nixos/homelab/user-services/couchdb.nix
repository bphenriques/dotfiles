{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ name, ... }: {
      options.services.couchdb = {
        enable = lib.mkEnableOption "CouchDB account for this user";
        databases = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          readOnly = true;
          default = [ "obsidiandb-${name}" ];
        };
      };
    })
  ];
}
