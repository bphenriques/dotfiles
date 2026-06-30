{ self, config, ... }:
{
  microvm = {
    autostart = [ "share-vm" ];
    vms.share-vm = {
      flake = self;
      restartIfChanged = true;
    };
  };
  # (The guest's tap is enslaved to the bridge in hosts/share-vm/microvm.nix's tap-up hook.)

  # Least-important, internet-facing load: throttled slice, low weight, hard-capped. The
  # rest is host-side containment of the unprivileged VMM — an escape stays confined to this
  # VM's own state. cloud-hypervisor is tap-native (no setuid helper), so dropping caps and
  # NoNewPrivileges doesn't break its networking.
  systemd.services."microvm@share-vm".serviceConfig = {
    # Hard limit resources
    Slice = "throttled.slice";
    CPUWeight = 10;
    CPUQuota = "100%";
    MemoryMax = "2G";

    # Isolate Filesystem
    ProtectSystem = "strict";
    ReadWritePaths = [ "${config.microvm.stateDir}/share-vm" ];
    PrivateTmp = true;
    ProtectHome = true;
    # Umask?

    # Privilege & Capabilities
    NoNewPrivileges = true;
    CapabilityBoundingSet = "";
    LockPersonality = true;
    # RestrictSUIDSGID

    # Kernel & System Protection
    ProtectClock = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    # ProtectProc = "invisible"; ?
    # ProcSubset = "pid";

    # FIXME? Device Isolation (Cloud-Hypervisor Specific)
    #PrivateDevices = true;
    #DeviceAllow = [
    #  "/dev/kvm rwm"
    #  "/dev/net/tun rwm"
    #];

    # Advanced
    RestrictNamespaces = true;            # the VMM runs no jailer, so creates none
    SystemCallArchitectures = "native";
    RestrictRealtime = true;
    # Tighter profile (Rust+KVM, no JIT; needs only the AF_UNIX API socket + the tap)
    MemoryDenyWriteExecute = true;
    RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK" ];
    SystemCallFilter = [ "@system-service" ];
  };

  selfhost.monitoring.scopes.share-vm = {
    scrapeConfigs = [
      {
        job_name = "share-vm";
        static_configs = [{
          targets = [ "${config.custom.fleet.computeMicrovm.hosts.share-vm.ip}:9100" ];
          labels.instance = "share-vm";
        }];
      }
      {
        job_name = "share-vm-traefik";
        static_configs = [{
          targets = [ "${config.custom.fleet.computeMicrovm.hosts.share-vm.ip}:9117" ];
          labels.instance = "share-vm";
        }];
      }
    ];
    rules = [{
      name = "share-vm";
      rules = [
        {
          alert = "ShareVmDown";
          expr = ''up{job="share-vm"} == 0'';
          for = "5m";
          labels.severity = "warning";
          annotations.summary = "share-vm is unreachable — file sharing is offline";
        }
        {
          alert = "ShareStorageNearCap";
          expr = ''node_filesystem_avail_bytes{mountpoint="/srv/share",fstype="ext4"} / node_filesystem_size_bytes{mountpoint="/srv/share",fstype="ext4"} < 0.1'';
          for = "15m";
          labels.severity = "warning";
          annotations.summary = "/srv/share is over 90% full — uploads will start failing";
        }
        {
          alert = "ShareVmHighCpu";
          expr = ''(1 - avg by(instance) (rate(node_cpu_seconds_total{instance="share-vm",mode="idle"}[5m]))) * 100 > 90'';
          for = "15m";
          labels.severity = "warning";
          annotations.summary = "share-vm CPU over 90% for 15m";
        }
      ];
    }];
  };
}
