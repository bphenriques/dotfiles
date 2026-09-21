{ agentVm, ... }:
{
  # Networking, tap and vsock come from the microvm-guest module (placement set by mkMicrovmGuest).
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 1536;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon before the OOM killer fires

    shares = [
      # The live vault on the NAS, reached through compute's CIFS mount. RW: the agent writes notes.
      # The mount forces gid 5000 with 0660, so the guest's own gid-5000 group is what grants access.
      # posixAcl drops `--posix-acl --xattr`, which CIFS cannot answer: with them on, even opendir
      # returns EOPNOTSUPP and the guest sees an unusable mount rather than a permission error.
      { source = "/mnt/homelab-bphenriques/notes"; mountPoint = agentVm.vaultRoot; tag = "vault"; proto = "virtiofs"; posixAcl = false; }
      { source = "/var/lib/agent-vm-secrets"; mountPoint = agentVm.secretsRoot; tag = "secrets"; proto = "virtiofs"; readOnly = true; } # API_SERVER_KEY env
    ];
    volumes = [
      { image = "hermes-state.img"; label = "hermes-state"; mountPoint = agentVm.stateRoot; size = 4096; } # host key + hermes state
    ];
  };
}
