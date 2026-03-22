{ lib, ... }:
{
  config.custom.homelab._userOptionExtensions = [
    ({ ... }: {
      options.services.wireguard = {
        enable = lib.mkEnableOption "Wireguard configuration for this user (requires Wireguard)";
        devices = lib.mkOption {
          type = lib.types.listOf (lib.types.submodule {
            options = {
              name = lib.mkOption { type = lib.types.strMatching "[a-z0-9][a-z0-9-]*"; description = "Device name (e.g., phone, laptop). Lowercase alphanumeric and dashes only."; };
              ip = lib.mkOption {
                type = lib.types.str;
                description = "Static WireGuard client IP (e.g., 10.100.0.42)";
              };
              fullAccess = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "If true, device can access entire network. If false, only the home server.";
              };
            };
          });
          default = [ ];
          description = "List of WireGuard devices for this user";
        };
      };
    })
  ];
}
