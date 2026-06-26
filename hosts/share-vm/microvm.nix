{ lib, pkgs, shareVm, ... }:
let
  inherit (import ../shared.nix) computeMicrovm dns;
  vmBridge = computeMicrovm.bridge;
  vmIp = computeMicrovm.hosts.share-vm;
  vmMac = "02:00:00:00:01:11";  # also referenced by microvm.interfaces below
  tapId = "vm-share";           # host tap; enslaved to the bridge in binScripts.tap-up
in
{
  networking = {
    useDHCP = false;
    useNetworkd = true;
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
    # cloud-hypervisor over Firecracker for live memory ballooning (see README); runs
    # unprivileged (microvm:kvm) under the host sandbox.
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;            # base footprint: Traefik + FileBrowser + tailscale
    balloon = true;        # live resize, no reboot: ch-remote resize --balloon
    deflateOnOOM = true;   # return memory under guest pressure

    # Guest CID for the AF_VSOCK control channel: lets cloud-hypervisor systemd-notify
    # readiness, so microvm@share-vm goes active only once the guest is actually up.
    vsock.cid = 3;

    # Plain tap (cloud-hypervisor can't use type = "bridge"); enslave it to compute's bridge
    # via microvm.nix's documented tap-up hook — appends to the generated tap-creation script,
    # so it runs on the host right after the tap comes up.
    interfaces = [{
      type = "tap";
      id = tapId;
      mac = vmMac;
    }];
    binScripts.tap-up = lib.mkAfter ''
      ${lib.getExe' pkgs.iproute2 "ip"} link set dev ${tapId} master ${vmBridge.name}
    '';

    # No host shares: /nix/store rides a read-only image (storeOnDisk), nothing mounted in.
    shares = [ ];

    # VM-owned block devices, labelled for stable mapping: shared files (size = hard cap),
    # state (host key + creds), and the Tailscale identity (keeps the Funnel URL stable).
    volumes = [
      { image = "share-data.img"; label = "share"; mountPoint = shareVm.filesRoot; size = 25 * 1024; }
      { image = "share-state.img"; label = "share-state"; mountPoint = shareVm.dataRoot; size = 1024; }
      { image = "tailscale-state.img"; label = "ts-state"; mountPoint = "/var/lib/tailscale"; size = 256; }
    ];
  };
}
