{ config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = cfg.services.home-assistant;
  inherit (config.systemd.services.home-assistant.serviceConfig) User Group;

  donglePath = "/dev/serial/by-id/usb-Nabu_Casa_ZBT-2_DCB4D90D1A20-if00";
  otbrDataDir = "/var/lib/otbr";
  configDir = config.services.home-assistant.configDir;
in
{
  custom.homelab.services.home-assistant = {
    displayName = "Home Assistant";
    metadata.description = "Home Automation";
    metadata.version = config.services.home-assistant.package.version;
    metadata.homepage = config.services.home-assistant.package.meta.homepage;
    metadata.category = "Home";
    port = 8123;
    subdomain = "home";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = false; # Home-Assistant mobile apps breaks. Relying on built-in authentication.
    healthcheck.path = "/manifest.json";
    integrations.homepage.enable = true;
    integrations.homepage.tab = "Home";
    backup = {
      package = pkgs.writeShellApplication {
        name = "backup-home-assistant";
        text = ''cp -a "${configDir}/backups/." "$OUTPUT_DIR/"'';
      };
      after = [ "home-assistant.service" ];
    };
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      "isal"
      "met"           # Weather
      "mobile_app"    # Companion app
      "zeroconf"      # Device discovery
      "dhcp"          # Device discovery

      # Thread / Matter (ZBT-2 dongle via OTBR container)
      "thread"
      "matter"
      "otbr"
    ];

    config = {
      default_config = {};
      homeassistant = {
        name = "Home";
        unit_system = "metric";
        time_zone = cfg.locale.timezone;
        latitude = cfg.locale.latitude;
        longitude = cfg.locale.longitude;
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
    "d ${otbrDataDir} 0755 root root -"
  ];

  # Matter controller: bridges HA to Matter/Thread devices via websocket (port 5580).
  services.matter-server.enable = true;

  # OpenThread Border Router: bridges the ZBT-2 dongle's Thread radio to the IP network.
  # Home Assistant connects to OTBR's REST API (port 8091) for Thread/Matter device management.
  virtualisation.oci-containers.containers.otbr = {
    # Upstream only publishes :latest and :main tags
    image = "openthread/border-router:latest";
    autoStart = true;

    environment = {
      OT_RCP_DEVICE = "spinel+hdlc+uart:///dev/ttyACM0?uart-baudrate=460800";
      OT_INFRA_IF = "bond0";
      OT_THREAD_IF = "wpan0";
      OT_REST_LISTEN_ADDR = "127.0.0.1";
      OT_REST_LISTEN_PORT = "8091";
      OT_LOG_LEVEL = "5";
      TZ = cfg.locale.timezone;
    };

    volumes = [ "${otbrDataDir}:/data" ];
    extraOptions = [
      "--network=host"
      "--device=${donglePath}:/dev/ttyACM0"
      "--device=/dev/net/tun:/dev/net/tun"
      "--cap-add=NET_ADMIN"
      "--cap-add=NET_RAW"
      "--memory=128m"
    ];
  };

  # Ensure OTBR starts after bond0 is up
  systemd.services.podman-otbr = {
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
  };

  systemd.services.home-assistant = {
    after = [ "podman-otbr.service" ];
    wants = [ "podman-otbr.service" ];
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
    "net.ipv6.conf.all.forwarding" = 1;
    "net.ipv6.conf.all.accept_ra" = 2;
    "net.ipv6.conf.all.accept_ra_rt_info_max_plen" = 64;
    "net.ipv6.conf.default.forwarding" = 1;
    "net.ipv6.conf.default.accept_ra" = 2;
  };
}
