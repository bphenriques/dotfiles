{
  authorizedSSHKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEfNK2CGbIOfCrFsuWsX8bxqod4vtRJYYXpO54NWUdIY android-phone"
  ];

  dns = "1.1.1.1";

  # Set using static DHCP IPs. Alternatively, I should have disabled DHCP for a specific range but here we are.
  lan = {
    subnet = "192.168.1.0/24";
    hosts = {
      laptop = "192.168.1.121";
      bruno-home-nas = "192.168.1.192";
      compute = "192.168.1.196";        # First one from link aggregation
      inky = "192.168.1.92";            # Raspberry Pi Zero 2W
      jetkvm = "192.168.1.195";
    };
  };

  # Microvm guests live on a compute-internal bridge; reachable from the fleet
  # only via compute (ProxyJump / Traefik), internet via NAT through bond0.
  microvm = {
    bridge = {
      name = "compute-microvm";   # 15-char IFNAMSIZ limit
      gateway = "10.20.1.1";
      prefixLength = 24;
    };
    hosts = {
      share-vm = "10.20.1.11";
    };
  };
}
