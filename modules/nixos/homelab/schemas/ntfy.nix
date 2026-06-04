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

        access = lib.mkOption {
          type = lib.types.enum [ "ro" "wo" "rw" ];
          default = "wo";
          description = ''
            ACL granted to this publisher on the primary `topic`. `wo`
            (default) is publish-only.
          '';
        };

        extraAccess = lib.mkOption {
          type = lib.types.attrsOf (lib.types.enum [ "ro" "wo" "rw" ]);
          default = { };
          description = ''
            Additional ACLs for this publisher on topics other than `topic`.
            Keyed by topic name. Useful when a single ntfy user (one token)
            needs to publish to one topic and subscribe to another, e.g.
            an adapter whose outbound goes to `personal-agent` and whose
            subscriber loop reads from `personal-agent-inbox`.
          '';
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
