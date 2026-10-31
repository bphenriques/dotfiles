# Allocation table: listed in ../../../microvms.nix, read by ../host.nix to drive the bridge and seal.
{
  bridge = {
    name = "ai-microvm";
    gateway = "10.20.2.1";
    prefixLength = 24;
  };

  guests = {
    agent-alfred-vm = {
      ip = "10.20.2.13";
      mac = "02:00:00:00:02:13";
      vsockCid = 5;
      serviceConfig.MemoryMax = "5G";   # guest RAM plus hypervisor overhead: the cap has to sit above microvm.mem
      # Egress holes are set in ./host.nix, where the ports they open are readable from config.
    };
  };
}
