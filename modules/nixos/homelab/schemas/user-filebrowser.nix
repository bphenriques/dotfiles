{ lib, ... }:
{
  options.services.filebrowser = {
    enable = lib.mkEnableOption "FileBrowser account for this user";
    scope = lib.mkOption {
      type = lib.types.str;
      default = "/shared";
      description = "Directory scope relative to FileBrowser root";
    };
    admin = lib.mkEnableOption "admin access";
    permissions = lib.mkOption {
      type = lib.types.nullOr (lib.types.attrsOf lib.types.bool);
      default = null;
      description = "Per-user permission overrides (null = inherit defaults)";
    };
  };
}
