# Guest-side microvm declaration: hypervisor flavour, virtual hardware,
# network interface, virtiofs shares, persistent volume.
#
# Host-side counterpart lives in hosts/compute/microvm.nix (bridge,
# firewall, microvm.vms.personal-agent = { flake = self; ... }).
{ ... }:
let
  inherit (import ../shared.nix) microvm;
  vmBridge = microvm.bridge;
  vmIp = microvm.hosts.personal-agent;
  vmMac = "02:00:00:00:01:10";  # also referenced by microvm.interfaces below
in
{
  # systemd-networkd + MAC-based match. Predictable-name interfaces
  # (`enp0sN`) from virtio-net would defeat a `networking.interfaces.eth0`
  # config; matching by MAC sidesteps that without falling back to the
  # legacy `net.ifnames=0` kernel knob.
  networking = {
    useDHCP = false;
    useNetworkd = true;
    firewall.enable = false;  # trust the bridge interface — only reachable from compute
  };
  systemd.network = {
    enable = true;
    networks."10-lan" = {
      matchConfig.MACAddress = vmMac;
      # [Network] section — `Address`, `Gateway`, `DNS` are the
      # canonical shorthand keys. Avoids the NixOS-version-specific
      # `address = [...]` / `routes = [...]` helpers.
      networkConfig = {
        Address = "${vmIp}/${toString vmBridge.prefixLength}";
        Gateway = vmBridge.gateway;
        DNS = "1.1.1.1 9.9.9.9";
      };
    };
  };

  microvm = {
    hypervisor = "qemu";
    vcpu = 2;
    # Avoid exactly 2048 — QEMU+KVM hangs on this value
    # (microvm.nix issue #171). 4 GB gives plenty of headroom for
    # Hermes (~300 MB idle, ~1.5 GB under load) + npx cache.
    mem = 4096;

    interfaces = [{
      type = "bridge";  # microvm.nix attaches the tap to `bridge` for us
      id = "vm-hermes";
      mac = vmMac;      # same value matched by systemd.network above
      bridge = vmBridge.name;
    }];

    # Read-only /nix/store from host — saves disk and avoids rebuilding
    # everything inside the VM.
    shares = [
      {
        source = "/nix/store";
        mountPoint = "/nix/.ro-store";
        tag = "ro-store";
        proto = "virtiofs";
      }
    ];

    # /var/lib/hermes persists across VM rebuilds: Honcho memory,
    # sessions, mcpvault npx cache, vault git clone.
    volumes = [{
      image = "hermes-var.img";
      mountPoint = "/var/lib/hermes";
      size = 4096;  # 4 GB — npx cache + memory files + Honcho data + vault clone
    }];
  };
}
