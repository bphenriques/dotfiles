# Allocation table: read by flake.nix to build each guest, and by ./host.nix to drive the bridge and seal.
{
  bridge = {
    name = "compute-microvm";   # 15-char IFNAMSIZ limit
    gateway = "10.20.1.1";
    prefixLength = 24;
  };

  guests = {
    cv-vm = {
      ip = "10.20.1.12";
      mac = "02:00:00:00:01:12";
      vsockCid = 4;
      serviceConfig = {
        Slice = "throttled.slice";
        CPUWeight = 10;
        CPUQuota = "100%";
        MemoryMax = "768M";
      };
      monitoring = {
        traefikMetrics = true;
      };
    };

    # hermes-agent fronting the ai host's Ollama; the chat UI runs on compute.
    agent-vm = {
      ip = "10.20.1.13";
      mac = "02:00:00:00:01:13";
      vsockCid = 5;
      serviceConfig = {
        Slice = "throttled.slice";
        CPUWeight = 10;
        CPUQuota = "100%";
        MemoryMax = "2G";
      };
      monitoring = {
        storageMount = "/var/lib/hermes";
      };
      # The one LAN hole in the seal: the ai host's Ollama.
      egress.allowLan = [{ host = "ai"; ports = [ 11434 ]; }];
    };
  };
}
