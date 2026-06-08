# ntfy: bundled, opinionated notify provider — the default backend behind `notify.package`.
# Runs the ntfy-sh server, wires the notify endpoint, and provisions topics/publisher tokens.
{ config, lib, pkgs, self, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.ntfy;
  inherit (cfg.notify) topics;

  notifyServices = lib.filterAttrs (_: s: s.integrations.notify.enable) cfg.services;
  servicePublishers = lib.mapAttrs (name: s: {
    inherit (s.integrations.notify) topic tokenFile;
    owner = name;
  }) notifyServices;

  notifyTasks = lib.filterAttrs (_: t: t.integrations.notify.enable) cfg.tasks;
  taskPublishers = lib.mapAttrs (name: t: {
    inherit (t.integrations.notify) topic tokenFile;
    owner = name;
  }) notifyTasks;

  allPublishers = servicePublishers // taskPublishers;

  configFile = pkgs.writeText "ntfy-configure.json" (builtins.toJSON {
    publicTopics = lib.attrNames (lib.filterAttrs (_: t: t.public) topics);
    publishers = allPublishers;
  });
in
{
  options.custom.homelab.notify.ntfy.enable =
    lib.mkEnableOption "ntfy notification implementation (server + provisioning)";

  config = lib.mkIf cfg.notify.ntfy.enable {
    custom.homelab = {
      services.ntfy = {
        displayName = "Ntfy";
        description = "Push Notifications";
        port = 2586;
        healthcheck.path = "/v1/health";
        integrations.homepage.enable = true;
        integrations.homepage.tab = "Admin";
      };

      notify.url = serviceCfg.url;

      runtimeSecrets.ntfy-admin-password = {
        restartUnits = [ "ntfy-sh.service" "ntfy-configure.service" ];
      };
    };

    services.ntfy-sh = {
      enable = true;
      settings = {
        base-url = serviceCfg.publicUrl;
        listen-http = "${serviceCfg.host}:${toString serviceCfg.port}";
        behind-proxy = true;
        auth-default-access = "deny-all";
        enable-login = true;
      };
    };

    systemd.services.ntfy-sh.serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };

    systemd.tmpfiles.rules = [
      "d /var/lib/homelab-secrets/notify-publishers 0711 root root -"
    ];

    systemd.services.ntfy-configure = {
      description = "ntfy setup";
      wantedBy = [ "ntfy-sh.service" ];
      after = [ "ntfy-sh.service" ];
      requires = [ "ntfy-sh.service" ];
      partOf = [ "ntfy-sh.service" ];
      restartTriggers = [ configFile self.packages.ntfy-manage ];
      startLimitIntervalSec = 300;
      startLimitBurst = 3;
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        TimeoutStartSec = 600;
        Restart = "on-failure";
        RestartSec = 10;
        UMask = "0077";
      };
      environment = {
        NTFY_ADMIN_PASSWORD_FILE = cfg.runtimeSecrets.ntfy-admin-password.path;
        NTFY_PROVISION_FILE = configFile;
      };
      script = lib.getExe self.packages.ntfy-manage;
    };
  };
}
