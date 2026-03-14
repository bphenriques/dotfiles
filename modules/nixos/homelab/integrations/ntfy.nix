{ lib, config, ... }:
let
  tokenDir = "/var/lib/homelab-secrets/ntfy-publishers";
  cfg = config.custom.homelab.ntfy;

  defaultTopics = {
    media.public = true;
    download.public = false;
    admin.public = false;
    backups.public = false;
  };

  mkNtfyIntegration = name: {
    options.integrations.ntfy = lib.mkOption {
      type = lib.types.submodule {
        options = {
          enable = lib.mkEnableOption "ntfy notifications";

          topic = lib.mkOption {
            type = lib.types.enum (lib.attrNames cfg.topics);
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
  options.custom.homelab.ntfy.topics = lib.mkOption {
    type = lib.types.attrsOf (lib.types.submodule {
      options.public = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether the topic can be published without authentication";
      };
    });
    default = defaultTopics;
    description = "Known ntfy topics and visibility settings";
  };

  config.custom.homelab._serviceOptionExtensions = [
    ({ name, ... }: mkNtfyIntegration name)
  ];

  config.custom.homelab._taskOptionExtensions = [
    ({ name, ... }: mkNtfyIntegration name)
  ];
}
