let
  computeGuests = import ./compute/microvm/guests.nix;
in
{
  ssh = {
    authorizedKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEfNK2CGbIOfCrFsuWsX8bxqod4vtRJYYXpO54NWUdIY android-phone"
    ];
  };

  dns = "1.1.1.1";

  # Set using static DHCP IPs. Alternatively, I should have disabled DHCP for a specific range but here we are.
  lan = {
    subnet = "192.168.1.0/24";
    hosts = {
      laptop = "192.168.1.121";
      compute = "192.168.1.196";        # First one from link aggregation
      storage = "192.168.1.199";
      inky = "192.168.1.92";            # Raspberry Pi Zero 2W
      jetkvm = "192.168.1.195";
    };
  };

  microvms.compute = builtins.mapAttrs (_: g: g.ip) computeGuests.guests; # Used to seed /etc/hosts and ssh jump

  # Required to define how transmission maps downloads to folders and enable declaretively adding them.
  media.downloadCategories = {
    radarr = "radarr";
    sonarr = "sonarr";
  };

  ai = {
    model = "qwen3.5:4b";
    extraModels = [ "gpt-oss:20b" ];
  };
}
