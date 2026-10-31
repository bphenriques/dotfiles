{ alfredVm, ... }:
{
  microvm = {
    hypervisor = "cloud-hypervisor";
    vcpu = 2;
    mem = 4096;
    balloon = true;        # virtio-balloon: host can reclaim guest memory the VM isn't using
    deflateOnOOM = true;   # on guest OOM, auto-deflate the balloon before the OOM killer fires

    shares = [
      # The live vault on the NAS, reached through compute's CIFS mount. RW: the agent writes notes.
      # The mount forces gid 5000 with 0660, so the guest's own gid-5000 group is what grants access.
      # posixAcl drops `--posix-acl --xattr`, which CIFS cannot answer: with them on, even opendir
      # returns EOPNOTSUPP and the guest sees an unusable mount rather than a permission error.
      { source = "/mnt/homelab-bphenriques/notes"; mountPoint = alfredVm.vaultRoot; tag = "vault"; proto = "virtiofs"; posixAcl = false; }
      # Same mechanism, gid 5001. The parent of library/, for its AGENTS.md and formatting.md.
      { source = "/mnt/homelab-media/recipes"; mountPoint = alfredVm.recipesRoot; tag = "recipes"; proto = "virtiofs"; posixAcl = false; }
      { source = "/var/lib/agent-alfred-vm-secrets"; mountPoint = alfredVm.secretsRoot; tag = "secrets"; proto = "virtiofs"; readOnly = true; } # API_SERVER_KEY env
    ];
    volumes = [
      { image = "hermes-state.img"; label = "hermes-state"; mountPoint = alfredVm.stateRoot; size = 4096; } # host key + hermes state
    ];
  };
}
