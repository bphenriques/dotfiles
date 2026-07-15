let
  # MicroVM guest placement is host-local (hosts/compute/guests.nix), not fleet data. The fleet
  # needs only name→bridge-IP for `ssh -J` resolution + known-hosts pinning; derive it from there.
  computeGuests = import ./compute/guests.nix;
in
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

  # Guests reached from the fleet only by `ssh -J compute` (internal bridge; internet egress via
  # NAT, never the LAN — see profiles/nixos/capabilities/microvm-host.nix). Name→IP only; rich placement is host-local.
  microvmHosts = builtins.mapAttrs (_: g: g.ip) computeGuests.guests;
}
