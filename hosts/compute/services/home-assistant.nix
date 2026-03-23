{ config, ... }:
let
  serviceCfg = config.custom.homelab.services.home-assistant;
in
{
  # TODO: Once I have ZBT-2 dongle:
  # 1. Find stable device path: `ls -l /dev/serial/by-id/`
  # 2. Uncomment thread/matter/bluetooth extraComponents below
  # 3. Set up OTBR as a container for Thread border routing
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
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      "isal"
      "met"           # Weather
      "mobile_app"    # Companion app
      "zeroconf"      # Device discovery
      "dhcp"          # Device discovery

      # Thread / Matter (requires dongle)
      # "thread"
      # "matter"
      # "bluetooth"
      # "otbr"
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
    user = "hass";
    group = "hass";
  in [
    "f ${dir}/automations.yaml 0644 ${user} ${group} -"
    "f ${dir}/scenes.yaml 0644 ${user} ${group} -"
    "f ${dir}/scripts.yaml 0644 ${user} ${group} -"
  ];

  systemd.services.home-assistant.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };
}
