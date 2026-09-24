# systemd hardening for the units the microvm host module creates.
{
  # Confines the unprivileged VMM to this VM's state; cloud-hypervisor is tap-native so empty caps don't break networking.
  vmSandbox = {
    ProtectSystem = "strict";
    PrivateTmp = true;
    ProtectHome = true;
    NoNewPrivileges = true;
    CapabilityBoundingSet = "";
    LockPersonality = true;
    RestrictSUIDSGID = true;
    ProtectClock = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    ProtectProc = "invisible";
    ProcSubset = "pid";
    DevicePolicy = "closed";                # keep host /dev but restrict to what the VMM opens
    DeviceAllow = [ "/dev/kvm rw" "/dev/net/tun rw" ];
    RestrictNamespaces = true;
    SystemCallArchitectures = "native";
    RestrictRealtime = true;
    MemoryDenyWriteExecute = true;
    RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK" ];
    SystemCallFilter = [ "@system-service" ];
  };

  # virtiofsd talks vhost-user straight to the guest, yet upstream runs it as root with every capability.
  # Not narrowed further: ProtectSystem=strict needs per-share ReadWritePaths, RestrictSUIDSGID breaks guest setuid files.
  virtiofsdSandbox = {
    CapabilityBoundingSet = [
      "CAP_SETPCAP"         # it drops its own caps at startup; without this it fails closed
      "CAP_SYS_ADMIN"       # unshare/pivot_root for its own sandbox
      "CAP_SYS_CHROOT"
      "CAP_CHOWN"
      "CAP_DAC_OVERRIDE"
      "CAP_DAC_READ_SEARCH"
      "CAP_FOWNER"
      "CAP_FSETID"
      "CAP_MKNOD"
      "CAP_SETGID"
      "CAP_SETUID"
      "CAP_SETFCAP"
    ];
    NoNewPrivileges = true;
    ProtectHome = true;     # no share source lives under /home
    ProtectClock = true;
    ProtectKernelModules = true;
    ProtectKernelTunables = true;
    RestrictRealtime = true;
  };
}
