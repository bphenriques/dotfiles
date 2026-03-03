{
  authorizedSSHKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
  ];

  # Set using static DHCP IPs
  hosts = {
    bruno-home-nas = "192.168.1.192";
    compute = "192.168.1.196";
  };
}
