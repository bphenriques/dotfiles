{
  authorizedSSHKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
  ];

  dns.cloudflare = "1.1.1.1";

  # Set using static DHCP IPs. Consider updating DHCP to not assign IPs within a specific range and then set it here.
  # It is a bit annoying as I need to boot first to check the MAC Address.
  networks = {
    main = {
      subnet = "192.168.1.0/24";
      hosts = {
        laptop = "192.168.1.121";
        bruno-home-nas = "192.168.1.192";
        compute = "192.168.1.196";        # First one from link aggregation
        inky = "192.168.1.197";           # Raspberry Pi Zero 2W - assign via DHCP reservation
        jetkvm = "192.168.1.195";
      };
    };
  };
}
