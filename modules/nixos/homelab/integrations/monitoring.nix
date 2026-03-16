{ lib, ... }:
{
  config.custom.homelab._serviceOptionExtensions = [
    (_: {
      options.integrations.monitoring = {
        enable = lib.mkEnableOption "healthcheck monitoring for this service" // {
          default = true;
        };
      };
    })
  ];
}
