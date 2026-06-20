_:
let
  inherit (import ../shared.nix) microvm dns;
  vmBridge = microvm.bridge;
  vmIp = microvm.hosts.share-vm;
  vmMac = "02:00:00:00:01:11";  # also referenced by microvm.interfaces below
in
{
  networking = {
    useDHCP = false;
    useNetworkd = true;
    nftables.enable = true;
    # SSH only from compute over the bridge (the laptop reaches it via
    # `ssh -J compute`). The public path is localhost-only (Funnel → Traefik :8080);
    # the tailnet (100.x) and internet match no rule.
    firewall = {
      enable = true;
      # SSH + metrics (node-exporter :9100, Traefik :9117), compute-over-the-bridge only.
      extraInputRules = "ip saddr ${vmBridge.gateway} tcp dport { 22, 9100, 9117 } accept";
    };
  };
  systemd.network = {
    enable = true;
    networks."10-lan" = {
      matchConfig.MACAddress = vmMac;  # virtio gives unpredictable enp0sN names; match by MAC
      networkConfig = {
        Address = "${vmIp}/${toString vmBridge.prefixLength}";
        Gateway = vmBridge.gateway;
        DNS = "${dns} 9.9.9.9";
      };
    };
  };

  microvm = {
    # cloud-hypervisor: minimal rust-vmm VMM (memory-safe, own seccomp) like
    # Firecracker, but supports live memory ballooning — Firecracker can't, which was
    # the dealbreaker. Still sealed: no shares, store on a read-only disk, data
    # VM-owned; runs unprivileged (microvm:kvm) under the host sandbox.
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;            # base footprint: Traefik + FileBrowser + tailscale
    balloon = true;        # live memory resize without a reboot (ch-remote resize --balloon)
    deflateOnOOM = true;   # hand memory back automatically if the guest is under pressure

    # Plain tap; compute enslaves it to the bridge (see hosts/compute/microvm.nix).
    interfaces = [{
      type = "tap";
      id = "vm-share";
      mac = vmMac;
    }];

    # No host shares: storeOnDisk is implied, so /nix/store rides a read-only image
    # instead of a virtiofs mount from compute. The box shares nothing in.
    shares = [ ];

    # All data the VM owns, on its own block devices (labelled for stable mapping):
    #   share        — the shared files; size is the hard storage cap (uploads fail
    #                  at the cap, physical use grows with data). Curated out to the
    #                  NAS over sshfs, so it needs no compute-side backup.
    #   share-state  — SSH host key (sops age identity) + per-scope credentials.
    #   ts-state     — Tailscale node identity, so the Funnel URL stays stable.
    volumes = [
      { image = "share-data.img"; label = "share"; mountPoint = "/srv/share"; size = 25 * 1024; }
      { image = "share-state.img"; label = "share-state"; mountPoint = "/var/lib/share"; size = 1024; }
      { image = "tailscale-state.img"; label = "ts-state"; mountPoint = "/var/lib/tailscale"; size = 256; }
    ];
  };
}
