{ lib, pkgs, self, config, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Output of nixos-generate-config --root /mnt
  ];

  hardware.enableRedistributableFirmware = true;  # Misc drivers

  # Disk
  services.fstrim.enable = true;  # Weekly TRIM for NVMe longevity
  services.smartd.enable = true;  # Disk health

  # RAM: ~8GB on 32GB RAM. OOM safety net without NVMe wear
  zramSwap = {
    enable = true;
    memoryPercent = 25;
    algorithm = "zstd";
  };

  # Graphics
  hardware.graphics = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver    # VAAPI driver (iHD). Only driver supporting Alder Lake-N
      pkgs.vpl-gpu-rt            # oneVPL GPU runtime (required for QSV)
      pkgs.intel-compute-runtime # OpenCL runtime. Required for HDR->SDR tonemapping (Jellyfin)
    ];
  };
  environment.systemPackages = [ pkgs.intel-gpu-tools ];
  boot.kernelParams = [
    "i915.enable_guc=3"                # Enable GuC/HuC firmware for better media scheduling
    "block.events_dfl_poll_msecs=0"     # Disable removable media polling (no optical drives)
  ];

  # Bonding: only bond0 gets DHCP, physical interfaces stay silent
  # Router DHCP reservation should use bond0's MAC (inherited from enp1s0)
  networking.useDHCP = false;
  networking.bonds.bond0 = {
    interfaces = [ "enp1s0" "enp2s0" ];
    driverOptions = {
      mode = "active-backup";
      primary = "enp1s0";
    };
  };
  networking.interfaces.bond0.useDHCP = true;

  # Wait for bond0 carrier before starting dhcpcd (bond takes a moment to come up at boot). This ensures the right IP.
  # Otherwise, it will fall back to to 169.254.x.x link-local address as DHCP is slow.
  networking.dhcpcd.extraConfig = "noipv4ll";
  systemd.services.dhcpcd = {
    after = [ "sys-subsystem-net-devices-bond0.device" ];
    wants = [ "sys-subsystem-net-devices-bond0.device" ];
  };

  # Power management
  powerManagement = {
    powertop.enable = true;            # Auto-tune power settings at boot
    cpuFreqGovernor = "powersave";     # Favor low frequencies, still allows turbo when needed
  };

  # UPS monitoring (NUT netclient).
  # Credentials: update `cat /etc/ups/upsd.users` on the Synology NAS and restart `synosystemctl restart ups-usb`
  power.ups = {
    enable = true;
    mode = "netclient";
    upsmon.monitor.synology = {
      system = "ups@${self.shared.networks.main.hosts.bruno-home-nas}";
      powerValue = 1;
      user = "compute";
      passwordFile = config.sops.secrets."upsmon/password".path;
      type = "secondary";
    };
  };
  sops.secrets."upsmon/password" = {
    mode = "0440";   # NUT exporter reads via SupplementaryGroups=keys
    group = "keys";
  };

  # Misc
  services.thermald.enable = true;   # Intel thermal daemon
  boot.blacklistedKernelModules = [
    "iwlwifi"    # WiFi (always on Ethernet)
    "btusb"      # Bluetooth USB
    "bluetooth"  # Bluetooth stack
    "i8042"      # PS/2 keyboard controller
    "serio"      # PS/2 serial I/O
  ];

  # Monitoring scopes
  custom.homelab.monitoring.scopes.node = {
    exporters.node = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9101;
      enabledCollectors = [ "hwmon" "rapl" "systemd" "thermal_zone" ];
    };
    scrapeConfigs = [{
      job_name = "node";
      static_configs = [{
        targets = [ "127.0.0.1:9101" ];
        labels.instance = config.networking.hostName;
      }];
    }];
    rules = [{
      name = "system";
      rules = [
        {
          alert = "HighCPU";
          expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{mode="idle"}[5m]))) * 100 > 90'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "CPU > 90%";
        }
        {
          alert = "HighMemory";
          expr = "(1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes) * 100 > 85";
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "Memory > 85%";
        }
        {
          alert = "DiskAlmostFull";
          expr = ''(1 - node_filesystem_avail_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"} / node_filesystem_size_bytes{mountpoint="/",fstype!~"tmpfs|overlay|squashfs"}) * 100 > 80'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "Disk > 80%";
        }
        {
          alert = "HighTemperature";
          expr = "max by(instance) (node_hwmon_temp_celsius) > 80";
          "for" = "2m";
          labels.severity = "critical";
          annotations.summary = "Temp > 80°C";
        }
        {
          alert = "NASStorageFull";
          expr = ''(1 - node_filesystem_avail_bytes{fstype="cifs"} / node_filesystem_size_bytes{fstype="cifs"}) * 100 > 85'';
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.mountpoint }} > 85%";
        }
      ];
    }];
    systemdOverrides."prometheus-node-exporter".serviceConfig = {
      CapabilityBoundingSet = [ "CAP_DAC_READ_SEARCH" ];
      AmbientCapabilities = [ "CAP_DAC_READ_SEARCH" ];
    };
  };

  custom.homelab.monitoring.scopes.smartctl = {
    exporters.smartctl = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9633;
    };
    scrapeConfigs = [{
      job_name = "smartctl";
      scrape_interval = "15m";
      static_configs = [{
        targets = [ "127.0.0.1:9633" ];
        labels.instance = config.networking.hostName;
      }];
    }];
    rules = [{
      name = "disk-health";
      rules = [
        {
          alert = "SMARTDiskUnhealthy";
          expr = "smartctl_device_smart_status == 0";
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.device }}: SMART unhealthy";
        }
        {
          alert = "SMARTHighWearLevel";
          expr = "smartctl_device_percentage_used > 80";
          "for" = "0m";
          labels.severity = "warning";
          annotations.summary = "{{ $labels.device }}: {{ $value }}% wear";
        }
        {
          alert = "SMARTCriticalWarning";
          expr = "smartctl_device_critical_warning > 0";
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "{{ $labels.device }}: SMART critical warning";
        }
      ];
    }];
  };

  custom.homelab.monitoring.scopes.ups = let
    upsmon = config.power.ups.upsmon.monitor.synology;
    parsed = builtins.match "([^@]+)@(.+)" upsmon.system;
    upsName = builtins.elemAt parsed 0;
    nutServer = builtins.elemAt parsed 1;
  in lib.mkIf (config.power.ups.enable && config.power.ups.upsmon.monitor ? synology) (
    assert lib.assertMsg (parsed != null)
      "power.ups.upsmon.monitor.synology.system must be \"ups@host\" (got: ${upsmon.system})";
  {
    exporters.nut = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9305;
      inherit nutServer;
      nutUser = upsmon.user;
      passwordPath = upsmon.passwordFile;
    };
    scrapeConfigs = [{
      job_name = "nut";
      metrics_path = "/ups_metrics";
      params.ups = [ upsName ];
      static_configs = [{
        targets = [ "127.0.0.1:9305" ];
        labels.instance = config.networking.hostName;
      }];
    }];
    rules = [{
      name = "ups";
      rules = [
        {
          alert = "UPSOnBattery";
          expr = ''network_ups_tools_ups_status{flag="OB"} == 1'';
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "UPS running on battery";
        }
        {
          alert = "UPSLowBattery";
          expr = ''network_ups_tools_ups_status{flag="LB"} == 1'';
          "for" = "0m";
          labels.severity = "critical";
          annotations.summary = "UPS battery low";
        }
        {
          alert = "NUTExporterDown";
          expr = ''up{job="nut"} == 0'';
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "NUT exporter unreachable";
        }
      ];
    }];
    systemdOverrides."prometheus-nut-exporter".serviceConfig.SupplementaryGroups = [ "keys" ];
  });
}
