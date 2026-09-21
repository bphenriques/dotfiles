{ config, shareVm, ... }:
let
  # The only volume that grows; the other two are fixed-size appliance state.
  dataVolume = { image = "share-data.img"; label = "share"; mountPoint = shareVm.filesRoot; size = 40 * 1024; };
in
{
  # Networking, tap and vsock come from the microvm-guest module (placement set by mkMicrovmGuest).
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon back before the OOM killer fires
    shares = [ ];          # None as the image contains everything (storeOnDisk).
    volumes = [
      dataVolume                                                                                           # Shared Data
      { image = "share-state.img"; label = "share-state"; mountPoint = shareVm.dataRoot; size = 1024; }     # State (host key and creds)
      { image = "tailscale-state.img"; label = "ts-state"; mountPoint = "/var/lib/tailscale"; size = 256; } # Tailscale Identity
    ];

    # microvm.nix only creates a *missing* image, so bumping `size` is otherwise inert. `-c` leaves
    # first-time creation to its mkfs; `>` only ever grows, so this can never truncate data away.
    preStart = ''
      ${config.microvm.vmHostPackages.coreutils}/bin/truncate -c -s '>${toString dataVolume.size}M' '${dataVolume.image}'
    '';
  };

  # Grow the filesystem to match: systemd-growfs resizes ext4 online, no-op once the sizes agree.
  fileSystems.${dataVolume.mountPoint}.autoResize = true;
}
