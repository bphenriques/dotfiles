{ name, lib, homelabCfg, ... }:
let
  tokenDir = "/var/lib/homelab-secrets/ntfy-publishers";
in
{
  options.integrations.ntfy = lib.mkOption {
    type = lib.types.submodule {
      options = {
        enable = lib.mkEnableOption "ntfy notifications";

        topic = lib.mkOption {
          type = lib.types.enum (lib.attrNames homelabCfg.ntfy.topics);
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
}
