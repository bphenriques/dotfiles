{ lib, pkgs, shareVm, ... }:
let
  inherit (import ../shared.nix) computeMicrovm dns;

  vmBridge = computeMicrovm.bridge;
  microVm = computeMicrovm.hosts.share-vm;
  tapId = "vm-share";           # host tap; enslaved to the bridge in binScripts.tap-up
  quad9DNS = "9.9.9.9";         # TODO: Potentially make it primary?
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
        DNS = "${dns} ${quad9DNS}";
      };
    };
  };

  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;
    balloon = true;        # Dynamically give back memory in case the VM does not need
    deflateOnOOM = true;   # Or, host can return memory to the VM if it is under memory pressure

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
