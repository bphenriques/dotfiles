{ lib, config, pkgs, ... }:
let
  # Tokens are created idempotently by ntfy-configure; stale tokens for removed services are harmless (write-only, topic-scoped).
  tokenDir = "/var/lib/homelab-secrets/ntfy-publishers";
  cfg = config.custom.homelab.ntfy;
  ntfyCfg = config.custom.homelab.services.ntfy;

  defaultTopics = {
    media.public = true;
    download.public = false;
    admin.public = false;
    backups.public = false;
    voice-assistant.public = false;
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

  tasksWithNtfy = lib.filterAttrs (_: task:
    task.integrations.ntfy.enable && task.systemdServices != []
  ) config.custom.homelab.tasks;

  notifyFailureScript = pkgs.writeShellScript "task-notify-failure" ''
    if [ "''${SERVICE_RESULT:-}" != "success" ]; then
      NTFY_TOKEN="$(tr -d '\n' < "$NTFY_TOKEN_FILE")"
      ${pkgs.curl}/bin/curl -fsS --max-time 10 \
        -H "Authorization: Bearer $NTFY_TOKEN" \
        -H "Title: Task Failed" \
        -H "Tags: x" \
        -H "Priority: high" \
        -d "$1 failed (''${SERVICE_RESULT:-unknown})" \
        "$NTFY_URL" || true
    fi
  '';

  mkFailureOverrides = _: task:
    let
      ntfy = task.integrations.ntfy;
      env = {
        NTFY_URL = "${ntfyCfg.url}/${ntfy.topic}";
        NTFY_TOKEN_FILE = ntfy.tokenFile;
      };
    in lib.listToAttrs (map (svc: lib.nameValuePair svc {
      environment = env;
      serviceConfig.ExecStopPost = "${notifyFailureScript} ${svc}";
    }) task.systemdServices);
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

  config.systemd.services = lib.mkMerge (lib.attrValues (lib.mapAttrs mkFailureOverrides tasksWithNtfy));
}
