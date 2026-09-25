let
  computeGuests = import ./compute/microvm/guests.nix;

  # Set using static DHCP IPs.  I should have disabled DHCP for a specific range but here we are.
  lan = {
    subnet = "192.168.1.0/24";
    hosts = {
      laptop = "192.168.1.121";
      compute = "192.168.1.196";
      storage = "192.168.1.199";
      ai = "192.168.1.200";
      inky = "192.168.1.92";
      jetkvm = "192.168.1.195";
    };
  };
in
{
  ssh = {
    authorizedKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEfNK2CGbIOfCrFsuWsX8bxqod4vtRJYYXpO54NWUdIY android-phone"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF7T3w1ex/9s2FBy8EM0HTSjF0kQIQi61ZwBh/mXQyNv ios-terminus"
    ];
  };

  dns = "1.1.1.1";

  inherit lan;

  microvms.compute = builtins.mapAttrs (_: g: g.ip) computeGuests.guests; # Used to seed /etc/hosts and ssh jump

  # Two facts that happen to share a spelling: the UPS as upsd names it, and the machine serving it.
  ups = {
    name = "storage";
    host = lan.hosts.storage;
  };

  # Required to define how transmission maps downloads to folders and enable declaretively adding them.
  media.downloadCategories = {
    radarr = "radarr";
    sonarr = "sonarr";
  };

  ai = {
    endpoint = {
      host = lan.hosts.ai;
      port = 11434;                     # Ollama's default, served from a container rather than nixpkgs
    };
    imageEndpoint = {
      host = lan.hosts.ai;
      port = 8000;                      # the toolbox image's own launcher port, kept so its docs still apply
    };
    model = "qwen3.6:35b-a3b";
    codingModel = "qwen3.8:27b";        # dense, so slower than the MoE above; picked for code quality
    embeddingModel = "embeddinggemma";  # 0.5 GB, so it sits alongside the chat weights
    contextLength = 65536;              # Hermes requires >=64K
  };
}
