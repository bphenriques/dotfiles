# Guest-side microvm declaration: hypervisor flavour, virtual hardware,
# network interface, virtiofs shares, persistent volume.
#
# Host-side counterpart lives in hosts/compute/microvm.nix (bridge,
# firewall, microvm.vms.hermes-vm = { flake = self; ... }).
{ config, ... }:
let
  vmIp = "10.20.1.10";
  hostBridgeIp = "10.20.1.1";
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
    firewall.enable = false;  # Phase 1: trust the bridge interface.
  };
  systemd.network = {
    enable = true;
    networks."10-lan" = {
      matchConfig.MACAddress = vmMac;
      # [Network] section — `Address`, `Gateway`, `DNS` are the
      # canonical shorthand keys. Avoids the NixOS-version-specific
      # `address = [...]` / `routes = [...]` helpers.
      networkConfig = {
        Address = "${vmIp}/24";
        Gateway = hostBridgeIp;
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
      bridge = "br-hermes";
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
      # Vault SMB mount, shared in from the host. Same absolute path so
      # SOUL.md / AGENTS.md references are unambiguous. Read-write for
      # phase 1; phase 2 splits this into RO vault + RW inbox/.
      {
        source = "/mnt/homelab-bphenriques";
        mountPoint = "/mnt/homelab-bphenriques";
        tag = "vault";
        proto = "virtiofs";
      }
      # Host-decrypted secrets for VM-side services (API server token,
      # eventually cloud-LLM credentials). The homelab framework renders
      # templates here on compute; the VM reads them via this read-only
      # share. See hosts/compute/services/hermes-vm-api.nix.
      {
        source = "/var/lib/microvm-secrets/hermes-vm";
        mountPoint = "/run/host-secrets";
        tag = "host-secrets";
        proto = "virtiofs";
      }
    ];

    # /var/lib/hermes persists across VM rebuilds: Honcho memory,
    # sessions, mcpvault npx cache.
    volumes = [{
      image = "hermes-var.img";
      mountPoint = "/var/lib/hermes";
      size = 4096;  # 4 GB — npx cache + memory files + Honcho data
    }];
  };
}
