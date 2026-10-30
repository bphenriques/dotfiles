{ config, lib, pkgs, utils, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.home-assistant;
  inherit (config.systemd.services.home-assistant.serviceConfig) User Group;

  donglePath = "/dev/serial/by-id/usb-Nabu_Casa_ZBT-2_DCB4D90D1A20-if00";
  dongleUnit = "${utils.escapeSystemdPath donglePath}.device";
  inherit (config.services.home-assistant) configDir;
in
{
  selfhost.services.home-assistant = {
    displayName = "Home Assistant";
    meta.homepage = "https://www.home-assistant.io";
    meta.description = "Home Automation";
    meta.category = "home";
    port = 8123;
    subdomain = "home";
    # Mobile apps break behind the gateway, so Home Assistant authenticates its own users. Its login
    # knows nothing of selfhost groups, so allowedGroups would not be enforced and is left unset.
    access.model = "native";
    healthcheck.path = "/manifest.json";
    integrations.homepage.group = "Services";
    backup = {
      package = pkgs.writeShellApplication {
        name = "backup-home-assistant";
        runtimeInputs = [ pkgs.coreutils ];
        text = ''
          shopt -s nullglob
          backups=("${configDir}/backups/"*.tar)
          if (( ''${#backups[@]} == 0 )); then
            echo "No Home Assistant backups found. Configure or repair the native backup scheduler." >&2
            exit 1
          fi
          cp -a "''${backups[@]}" "$OUTPUT_DIR/"
        '';
      };
      after = [ "home-assistant.service" ];
    };
    extraConfig.landingPage.enable = true;
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      "isal"
      "met"           # Weather
      "mobile_app"    # Companion app
      "zeroconf"      # Device discovery
      "dhcp"          # Device discovery

      # Thread / Matter (ZBT-2 dongle via OTBR)
      "thread"
      "matter"
      "otbr"
    ];

    config = {
      default_config = {};
      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = config.fleet.locale.timezone;
        inherit (config.fleet.locale) latitude;
        inherit (config.fleet.locale) longitude;
        external_url = serviceCfg.publicUrl;
        internal_url = serviceCfg.url;
      };

      http = {
        server_host = [ "127.0.0.1" "::1" ];
        server_port = serviceCfg.port;
        use_x_forwarded_for = true;
        trusted_proxies = [ "127.0.0.1" "::1" ];
      };

      # UI-managed files
      "automation ui" = "!include automations.yaml";
      "scene ui" = "!include scenes.yaml";
      "script ui" = "!include scripts.yaml";
    };
  };

  systemd.tmpfiles.rules = [
    "f ${configDir}/automations.yaml 0644 ${User} ${Group} -"
    "f ${configDir}/scenes.yaml 0644 ${User} ${Group} -"
    "f ${configDir}/scripts.yaml 0644 ${User} ${Group} -"
  ];

  # Matter controller: bridges HA to Matter/Thread devices via websocket (port 5580).
  services.matter-server.enable = true;

  # OpenThread Border Router: bridges the ZBT-2 dongle's Thread radio to the IP network.
  # Home Assistant connects to OTBR's REST API (port 8091) for Thread/Matter device management.
  services.openthread-border-router = {
    enable = true;
    backboneInterfaces = [ "bond0" ];
    logLevel = "notice";
    rest.listenPort = 8091;
    radio = {
      device = donglePath;
      baudRate = 460800;
    };
  };

  # Gate OTBR on the dongle being plugged: the .device unit starts it on hotplug
  # (wantedBy), bindsTo stops it on unplug, so it never restart-loops when absent.
  systemd.services.otbr-agent = {
    after = [ dongleUnit ];
    bindsTo = [ dongleUnit ];
    wantedBy = lib.mkForce [ dongleUnit ];
  };

  systemd.services.home-assistant = {
    after = [ "otbr-agent.service" ];
    wants = [ "otbr-agent.service" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = "10s";
      RestartMaxDelaySec = "5min";
      RestartSteps = 5;
    };
  };

  # IPv6 forwarding required for Thread border routing
  boot.kernel.sysctl = {
    "net.ipv6.conf.all.disable_ipv6" = 0;
    "net.ipv6.conf.all.accept_ra" = 2;
    "net.ipv6.conf.all.accept_ra_rt_info_max_plen" = 64;
    "net.ipv6.conf.default.forwarding" = 1;
    "net.ipv6.conf.default.accept_ra" = 2;
  };
}
