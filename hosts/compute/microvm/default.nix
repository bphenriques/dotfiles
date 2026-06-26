{ self, inputs, config, ... }:
let
  inherit (config.custom.fleet.computeMicrovm) bridge;
in
{
  imports = [ inputs.microvm.nixosModules.host ./share-vm.nix ./firewall.nix ];

  # Narrow override of the no-forwarding baseline: reach guests via `ssh -J compute` (admin).
  services.openssh.extraConfig = ''
    Match User ${config.users.users.bphenriques.name}
      AllowTcpForwarding yes
  '';

  microvm.autostart = [ "share-vm" ];
  microvm.vms.share-vm = {
    flake = self;
    restartIfChanged = true;
  };
  # (The guest's tap is enslaved to the bridge in hosts/share-vm/microvm.nix's tap-up hook.)

  # Least-important, internet-facing load: throttled slice, low weight, hard-capped. The
  # rest is host-side containment of the unprivileged VMM — an escape stays confined to this
  # VM's own state. cloud-hypervisor is tap-native (no setuid helper), so dropping caps and
  # NoNewPrivileges doesn't break its networking.
  systemd.services."microvm@share-vm".serviceConfig = {
    Slice = "throttled.slice";
    CPUWeight = 10;
    CPUQuota = "100%";
    MemoryMax = "2G";

    NoNewPrivileges = true;
    CapabilityBoundingSet = "";
    ProtectSystem = "strict";
    ReadWritePaths = [ "${config.microvm.stateDir}/share-vm" ];
    PrivateTmp = true;
    ProtectHome = true;
    ProtectClock = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectControlGroups = true;
    RestrictNamespaces = true;            # the VMM runs no jailer, so creates none
    SystemCallArchitectures = "native";
    RestrictRealtime = true;
    LockPersonality = true;

    # Tighter profile (Rust+KVM, no JIT; needs only the AF_UNIX API socket + the tap). If
    # microvm@share-vm won't start, peel this block back first, then ProtectSystem.
    MemoryDenyWriteExecute = true;
    RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK" ];
    SystemCallFilter = [ "@system-service" ];
  };

  # Bridge topology; the VM is not trusted, so it gets no input to compute's services —
  # compute reaches it by initiating (SSH, scrape). Egress policy is in ./firewall.nix.
  networking = {
    bridges.${bridge.name}.interfaces = [ ];
    interfaces.${bridge.name}.ipv4.addresses = [{
      address = bridge.gateway;
      inherit (bridge) prefixLength;
    }];
  };
}
