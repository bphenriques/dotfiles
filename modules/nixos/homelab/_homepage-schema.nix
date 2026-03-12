# Per-service homepage integration contract (options only).
# Imported by services-registry.nix, consumed by homepage host configuration.
{ lib, serviceName }:
let
  categoryType = lib.types.enum [
    "Media"
    "Admin"
    "Productivity"
    "Development"
    "Infrastructure"
  ];
in
{ ... }: {
  options = {
    enable = lib.mkEnableOption "homepage entry for this service";

    category = lib.mkOption {
      type = categoryType;
      description = "Homepage category/tab for this service";
    };

    description = lib.mkOption {
      type = lib.types.str;
      description = "Short description shown on homepage";
    };

    icon = lib.mkOption {
      type = lib.types.str;
      default = "${serviceName}.svg";
      description = "Icon name from dashboard-icons (e.g. 'miniflux.svg')";
    };

    siteMonitor = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Enable health monitoring for this service";
    };
  };
}
