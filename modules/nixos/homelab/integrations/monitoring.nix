{ lib, ... }:
{
  config.custom.homelab._serviceOptionExtensions = [
    (_: {
      options.integrations.monitoring = {
        enable = lib.mkEnableOption "monitoring for this service" // {
          default = true;
        };

        healthcheck = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to auto-generate a blackbox healthcheck probe for this service";
        };

        exporters = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          default = { };
          description = "Custom Prometheus exporters for this service";
        };

        scrapeConfigs = lib.mkOption {
          type = lib.types.listOf lib.types.attrs;
          default = [ ];
          description = "Custom Prometheus scrape configurations";
        };

        rules = lib.mkOption {
          type = lib.types.listOf lib.types.attrs;
          default = [ ];
          description = "Custom Prometheus alert rule groups";
        };

        systemdOverrides = lib.mkOption {
          type = lib.types.attrsOf lib.types.anything;
          default = { };
          description = "Systemd service overrides for monitoring-related units";
        };
      };
    })
  ];
}
