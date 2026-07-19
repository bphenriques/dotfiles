{ cvVm, ... }:
{
  # Networking, tap and vsock come from the microvm-guest.nix profile (from injected guestPlacement).
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 1;
    mem = 512;
    shares = [ ];          # None as the static site is baked into the image (storeOnDisk).
    volumes = [
      { image = "cv-state.img"; label = "cv-state"; mountPoint = cvVm.dataRoot; size = 512; }        # State (host key)
      { image = "tailscale-state.img"; label = "ts-state"; mountPoint = "/var/lib/tailscale"; size = 256; } # Tailscale Identity
    ];
  };
}
