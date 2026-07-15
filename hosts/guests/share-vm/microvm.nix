{ shareVm, ... }:
{
  # Networking, tap and vsock come from the microvm-guest.nix profile (from injected guestPlacement).
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon back before the OOM killer fires
    shares = [ ]; # No host shares. The image contains everything (storeOnDisk).
    volumes = [
      { image = "share-data.img"; label = "share"; mountPoint = shareVm.filesRoot; size = 25 * 1024; }      # Shared Data
      { image = "share-state.img"; label = "share-state"; mountPoint = shareVm.dataRoot; size = 1024; }     # State (host key and creds)
      { image = "tailscale-state.img"; label = "ts-state"; mountPoint = "/var/lib/tailscale"; size = 256; } # Tailscale Identity
    ];
  };
}
