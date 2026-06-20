{ self, inputs, config, pkgs, ... }:
let
  inherit (config.custom.fleet.microvm) bridge;
in
{
  imports = [ inputs.microvm.nixosModules.host ./share-vm.nix ];

  # Narrow override of the hardened no-forwarding baseline: lets me reach guests
  # with `ssh -J compute bphenriques@<vm>` (admin / credential retrieval).
  services.openssh.extraConfig = ''
    Match User ${config.users.users.bphenriques.name}
      AllowTcpForwarding yes
  '';

  microvm.autostart = [ "share-vm" ];
  microvm.vms.share-vm = {
    flake = self;
    restartIfChanged = true;
  };

  # cloud-hypervisor uses a plain tap; enslave it to the microvm bridge once tap-up has
  # created it (compute uses scripted networking, so do it here rather than networkd).
  systemd.services."microvm-tap-interfaces@share-vm".serviceConfig.ExecStartPost =
    "${pkgs.iproute2}/bin/ip link set vm-share master ${bridge.name}";

  # Public share VM shares the throttled slice with the family services but yields to
  # them (low weight) and is hard-capped — it's the least-important, internet-facing
  # load. The rest is host-side containment of the (already unprivileged microvm:kvm)
  # VMM: a hypervisor escape is confined to this VM's own state, not the rest of
  # compute. cloud-hypervisor is tap-native — no setuid bridge helper — so dropping
  # caps and NoNewPrivileges don't break its networking.
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

    # Tighter profile — deploy-test these together. cloud-hypervisor is Rust + KVM (no
    # JIT) and only needs the AF_UNIX API socket plus the pre-created tap. If
    # microvm@share-vm fails to start, peel this block back first (then ProtectSystem).
    MemoryDenyWriteExecute = true;
    RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" "AF_NETLINK" ];
    SystemCallFilter = [ "@system-service" ];
  };

  networking = {
    bridges.${bridge.name}.interfaces = [ ];
    interfaces.${bridge.name}.ipv4.addresses = [{
      address = bridge.gateway;
      inherit (bridge) prefixLength;
    }];

    # Outbound only: NAT forwards the VM to the internet (tailscale/ACME). The bridge
    # is deliberately NOT trusted — the insecure-by-default VM gets no input access to
    # compute's own services; compute reaches the VM by initiating (SSH, scrape).
    nat = {
      enable = true;
      internalInterfaces = [ bridge.name ];
      externalInterface = "bond0";
    };

    # Egress firewall: the VM reaches the internet but NOT the LAN. Without this a
    # compromised FileBrowser (app RCE, no hypervisor escape needed) could pivot
    # through NAT to the NAS and the rest of the fleet. The VM needs no LAN access —
    # curation is laptop→VM, and compute always initiates toward the VM.
    firewall.extraForwardRules = ''
      iifname "${bridge.name}" ip daddr { 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16 } drop comment "sealed VM: internet only, no LAN pivot"
    '';
  };
}
