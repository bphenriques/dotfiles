{ config, lib, pkgs, shareVm, ... }:
let
  inherit (import ../shared.nix) computeMicrovm dns;

  vmBridge = computeMicrovm.bridge;
  microVm = computeMicrovm.hosts.${config.networking.hostName};  # this guest's ip/mac/vsockCid
  tapId = "vm-share";           # host tap; enslaved to the bridge in binScripts.tap-up
in
{
  networking = {
    useDHCP = false;      # Disable legacy stack
    useNetworkd = true;   # Enable modern stack
  };
  systemd.network = {
    enable = true;
    networks."10-lan" = {
      matchConfig.MACAddress = microVm.mac;  # virtio gives unpredictable enp0sN names; match by MAC
      networkConfig = {
        Address = "${microVm.ip}/${toString vmBridge.prefixLength}";
        Gateway = vmBridge.gateway;
        DNS = dns;
        LinkLocalAddressing = "ipv4";  # v4-only guest: no fe80:: and no RA-assigned v6, so the
        IPv6AcceptRA = false;          # host's v4-only LAN-drop seal can't be sidestepped over v6
      };
    };
  };

  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon back before the OOM killer fires

    # Plugin Virtual Ethernet Cable (TAP) onto the network bridge as soon as the tap is available
    interfaces = [{ type = "tap"; id = tapId; mac = microVm.mac; }];
    binScripts.tap-up = lib.mkAfter ''
      ${lib.getExe' pkgs.iproute2 "ip"} link set dev ${tapId} master ${vmBridge.name}
    '';

    vsock.cid = microVm.vsockCid; # For readiness systemd integration
    shares = [ ]; # No host shares. The image contains everything (storeOnDisk).
    volumes = [
      { image = "share-data.img"; label = "share"; mountPoint = shareVm.filesRoot; size = 25 * 1024; }      # Shared Data
      { image = "share-state.img"; label = "share-state"; mountPoint = shareVm.dataRoot; size = 1024; }     # State (host key and creds)
      { image = "tailscale-state.img"; label = "ts-state"; mountPoint = "/var/lib/tailscale"; size = 256; } # Tailscale Identity
    ];
  };
}
