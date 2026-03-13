{ lib, ... }:
let
  tokenDir = "/var/lib/homelab-secrets/ntfy-publishers";

  mkNtfyIntegration = name: {
    options.integrations.ntfy = lib.mkOption {
      type = lib.types.submodule {
        options = {
          enable = lib.mkEnableOption "ntfy notifications";

          topic = lib.mkOption {
            type = lib.types.str;
            description = "Notification topic this service/task publishes to";
          };

          tokenFile = lib.mkOption {
            type = lib.types.str;
            default = "${tokenDir}/${name}";
            readOnly = true;
            description = "Path to the generated access token file for this publisher";
          };
        };
      };
      default = { };
      description = "ntfy notification integration";
    };
  };
in
{
  config.custom.homelab._serviceOptionExtensions = [
    ({ name, ... }: mkNtfyIntegration name)
  ];

  config.custom.homelab._taskOptionExtensions = [
    ({ name, ... }: mkNtfyIntegration name)
  ];
}
