{ alfredVm, ... }:
{
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 4096;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon before the OOM killer fires

    volumes = [
      { image = "hermes-state.img"; label = "hermes-state"; mountPoint = alfredVm.stateRoot; size = 4096; } # host key + hermes state
    ];
  };
}
