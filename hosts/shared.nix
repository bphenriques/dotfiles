{
  ssh = {
    authorizedKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEfNK2CGbIOfCrFsuWsX8bxqod4vtRJYYXpO54NWUdIY android-phone"
    ];

    # Host public keys for known-hosts pinning (public; replaces TOFU). Capture once at a
    # host's bootstrap: ssh-keyscan -t ed25519 <host>. share-vm's doubles as its sops age
    # identity (dotfiles-private). A key rotation is a deliberate edit here, never a silent
    # mount/SSH failure.
    hostKeys = {
      compute  = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFOGTI4xCPsaWL5OASOh+cRDTKVVg9aioxo0eQfnGmry";
      share-vm = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIArF1q4AmXnUZIDirGyY8JLxD8lFhIvrrPMjZ0avZR/G";
    };
  };

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

  # Microvm guests sit on a compute-internal bridge: reached from the fleet only by
  # initiating from compute (ssh -J), egress to the internet via NAT through bond0, never
  # the LAN (see hosts/compute/microvm/firewall.nix).
  computeMicrovm = {
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
