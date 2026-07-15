{
  bridge = {
    name = "compute-microvm";   # 15-char IFNAMSIZ limit
    gateway = "10.20.1.1";
    prefixLength = 24;
  };

  guests = {
    share-vm = {
      ip = "10.20.1.11";
      mac = "02:00:00:00:01:11";
      vsockCid = 3;
      autostart = true;
      serviceConfig = { Slice = "throttled.slice"; CPUWeight = 10; CPUQuota = "100%"; MemoryMax = "2G"; };
      monitoring = { traefikMetrics = true; storageMount = "/srv/share"; };
    };
  };
}
