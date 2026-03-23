{ config, pkgs, ... }:
let
  serviceCfg = config.custom.homelab.services.home-assistant;
  donglePath = "/dev/serial/by-id/usb-Nabu_Casa_ZBT-2_DCB4D90D1A20-if00";
  otbrDataDir = "/var/lib/otbr";
in
{
  custom.homelab.services.home-assistant = {
    displayName = "Home Assistant";
    metadata.description = "Home Automation";
    metadata.version = config.services.home-assistant.package.version;
    metadata.homepage = config.services.home-assistant.package.meta.homepage;
    metadata.category = "General";
    port = 8123;
    subdomain = "home";
    access.allowedGroups = [ config.custom.homelab.groups.admin ];
    forwardAuth.enable = true;
    healthcheck.path = "/manifest.json";
    integrations.homepage.enable = true;
    integrations.homepage.tab = "Admin";
    backup = {
      package = pkgs.writeShellApplication {
        name = "backup-home-assistant";
        text = ''
          cp -a "${config.services.home-assistant.configDir}/backups/." "$OUTPUT_DIR/"
        '';
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
        time_zone = config.time.timeZone;
        # Lisbon city center (not a personal address)
        latitude = 38.736946;
        longitude = -9.142685;
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

  # Ensure UI-managed YAML files exist on first boot
  systemd.tmpfiles.rules = let
    dir = config.services.home-assistant.configDir;
    inherit (config.systemd.services.home-assistant.serviceConfig) User Group;
  in [
    "f ${dir}/automations.yaml 0644 ${User} ${Group} -"
    "f ${dir}/scenes.yaml 0644 ${User} ${Group} -"
    "f ${dir}/scripts.yaml 0644 ${User} ${Group} -"
    "d ${otbrDataDir} 0755 root root -"
  ];

  # Matter controller: bridges HA to Matter/Thread devices via websocket (port 5580).
  services.matter-server.enable = true;

  # OpenThread Border Router: bridges the ZBT-2 dongle's Thread radio to the IP network.
  # Home Assistant connects to OTBR's REST API (port 8091) for Thread/Matter device management.
  virtualisation.oci-containers.containers.otbr = {
    image = "openthread/border-router:latest";
    autoStart = true;

    environment = {
      OT_RCP_DEVICE = "spinel+hdlc+uart:///dev/ttyACM0?uart-baudrate=460800";
      OT_INFRA_IF = "bond0";
      OT_THREAD_IF = "wpan0";
      OT_REST_LISTEN_ADDR = "127.0.0.1";
      OT_REST_LISTEN_PORT = "8091";
      OT_LOG_LEVEL = "5";
      TZ = config.time.timeZone;
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
