{ agentVm, ... }:
{
  # Networking, tap and vsock come from the microvm-guest.nix profile (from injected guestPlacement).
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon before the OOM killer fires

    # Both RO virtiofs of compute-owned data (RO enforced host-side by the VMM sandbox).
    shares = [
      { source = "/var/lib/agent-vm-vault"; mountPoint = agentVm.vaultRoot; tag = "vault"; proto = "virtiofs"; } # gitea vault clone
      { source = "/var/lib/agent-vm-secrets"; mountPoint = agentVm.secretsRoot; tag = "secrets"; proto = "virtiofs"; } # API_SERVER_KEY env
    ];
    volumes = [
      { image = "hermes-state.img"; label = "hermes-state"; mountPoint = agentVm.stateRoot; size = 4096; } # host key + hermes state
    ];
  };
}
