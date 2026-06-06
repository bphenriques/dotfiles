{ name, lib, ... }:
{
  options.integrations.homepage = lib.mkOption {
    type = lib.types.submodule {
      options = {
        enable = lib.mkEnableOption "homepage entry for this service";

        tab = lib.mkOption {
          type = lib.types.enum [ "Home" "Admin" ];
          default = "Home";
          description = "Homepage tab to display this service on";
        };

        icon = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = "${name}.svg";
          description = "Icon name from dashboard-icons (e.g. 'miniflux.svg')";
        };

        extraConfig = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          description = "Extra homepage configuration (widgets, etc.)";
        };
      };
    };
    default = { };
    description = "Homepage dashboard integration";
  };
}
