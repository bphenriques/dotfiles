# Notify subsystem (contract): topic registry + the per-task failure-notification wiring.
# Delivery is delegated to `notify.package` (the send-notification command), so the backend is swappable.
{ lib, config, pkgs, self, ... }:
let
  notifyCfg = config.custom.homelab.notify;
  sendNotification = "${notifyCfg.package}/bin/send-notification";

  tasksWithNotify = lib.filterAttrs (_: task:
    task.integrations.notify.enable && task.systemdServices != [ ]
  ) config.custom.homelab.tasks;

  notifyFailureScript = pkgs.writeShellScript "task-notify-failure" ''
    if [ "''${SERVICE_RESULT:-}" != "success" ]; then
      ${sendNotification} --topic "$NOTIFY_TOPIC" --title "Task Failed" \
        --message "$1 failed (''${SERVICE_RESULT:-unknown})" --priority high --tags x || true
    fi
  '';

  mkFailureOverrides = _: task:
    let
      inherit (task.integrations) notify;
      env = {
        NOTIFY_URL = notifyCfg.url;
        NOTIFY_TOPIC = notify.topic;
        NOTIFY_TOKEN_FILE = notify.tokenFile;
      };
    in lib.listToAttrs (map (svc: lib.nameValuePair svc {
      environment = env;
      serviceConfig.ExecStopPost = lib.mkAfter [ "${notifyFailureScript} ${svc}" ];
    }) task.systemdServices);
in
{
  options.custom.homelab.notify = {
    topics = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule {
        options.public = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether the topic can be published without authentication";
        };
      });
      default = { };
      description = ''
        Notification topics and visibility. Consumer-declared; framework subsystems
        (backup, alertmanager) self-register their own homelab-* topics via mkDefault.
      '';
    };

    url = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Base URL of the notification endpoint; set by the active notify provider, consumed by send-notification (NOTIFY_URL).";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.send-notification;
      defaultText = lib.literalExpression "self.packages.send-notification";
      description = ''
        send-notification implementation. Contract:
          send-notification --topic <t> --message <m> [--title <T>] [--priority <p>] [--tags <x>]
        Reads NOTIFY_URL and NOTIFY_TOKEN_FILE from the environment.
        topic and priority are the routing keys; per-medium routing (use-case -> channel) is an
        adapter/gateway concern, not the framework's. The framework's notification seam: used by
        backup, task-failure hooks, and simple service hooks (e.g. transmission). Swap this package
        to retarget all of them at once (e.g. an Apprise/gotify gateway that fans out). Producers
        that abstract notifications
        themselves — Alertmanager and *arr native connectors — stay native and are retargeted
        in their own config, not here.
      '';
    };
  };

  config.systemd.services = lib.mkMerge (lib.attrValues (lib.mapAttrs mkFailureOverrides tasksWithNotify));
}
