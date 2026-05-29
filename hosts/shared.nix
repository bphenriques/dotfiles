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

  # Microvm guests hosted on compute. The bridge is internal to compute;
  # VMs are reachable from the rest of the fleet only via compute as a
  # jump host (SSH ProxyJump, Traefik reverse-proxy).
  microvm = {
    bridge = {
      name = "compute-microvm";   # interface name on compute (also the iifname in nftables)
      gateway = "10.20.1.1";      # compute's IP on the bridge
      prefixLength = 24;          # subnet is implicitly 10.20.1.0/24
    };
    hosts = {
      hermes-vm = "10.20.1.10";
    };
  };
}
