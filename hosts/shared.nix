let
  computeGuests = import ./compute/microvm/guests.nix;

  # Static DHCP IPs. I should have disabled DHCP but here we are.
  lan = {
    subnet = "192.168.1.0/24";
    gateway = "192.168.1.1";
    hosts = {
      laptop = "192.168.1.121";
      compute = "192.168.1.196";
      storage = "192.168.1.199";
      ai = "192.168.1.200";
      jetkvm = "192.168.1.195";
    };
  };
in
{
  inherit lan;

  dns = "1.1.1.1";

  ssh = {
    authorizedKeys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3 laptop"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF7T3w1ex/9s2FBy8EM0HTSjF0kQIQi61ZwBh/mXQyNv ios-terminus"
    ];
  };
  ups = {
    name = "storage";
    host = lan.hosts.storage;
  };
  microvms.compute = builtins.mapAttrs (_: g: g.ip) computeGuests.guests; # Used to seed /etc/hosts and ssh jump

  # Required to define how transmission maps downloads to folders and enable declaratively adding them.
  downloadCategories = {
    radarr = "radarr";
    sonarr = "sonarr";
  };

  # Topic is the only thing a subscriber can mute, so these divide by how urgently they want reading
  # rather than by which subsystem produced them. Only media is readable without an account.
  notifyTopics = {
    admin = false;
    backup = false;
    download = false;
    media = true;
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
    speechEndpoint = {
      host = lan.hosts.ai;
      port = 8090;                      # whisper-server's default 8080 is the ad-hoc experiments port
    };
    model = "qwen3.6:35b-a3b";
    uncensoredModel = "huggingface.co/orcarouter/Qwen3.8-27B-Uncensored-GGUF";
    codingModel = "qwen3.8:27b";
    embeddingModel = "embeddinggemma";
    contextLength = 65536;              # Hermes requires >=64K
  };
}
