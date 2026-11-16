let
  microvmTables = import ./microvms.nix;

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
    ];
  };
  ups = {
    name = "storage";
    host = lan.hosts.storage;
  };
  microvms = builtins.mapAttrs (_: table: builtins.mapAttrs (_: g: g.ip) table.guests) microvmTables; # Used to seed /etc/hosts and ssh jump

  # Required to define how transmission maps downloads to folders and enable declaratively adding them.
  downloadCategories = {
    radarr = "radarr";
    sonarr = "sonarr";
  };

  # Split by urgency, the only thing a subscriber can mute, plus public for sharing. Only media and public are readable without an account.
  notifyTopics = {
    admin = false;
    backup = false;
    download = false;
    media = true;
    public = true;
  };

  ai = {
    endpoint = {
      host = lan.hosts.ai;
      port = 11434;
    };
    speechEndpoint = {
      host = lan.hosts.ai;
      port = 8090;
    };
    assistantEndpoint = {
      host = microvmTables.ai.guests.agent-alfredo-vm.ip;
      port = 8642;
    };
    model = "qwen3.8-flash-next";
    uncensoredModel = "qwen3.8-27b-uncensored";
    imageModel = "qwen-image-2.1-turbo";
    embeddingModel = "embeddinggemma-2";
    contextLength = 131072;             # per session; Hermes needs >=64K, 256K x2 overflows the GPU cap
  };

  forge.sshEndpoint = {
    host = lan.hosts.compute;
    port = 2222;                        # gitea's built-in SSH, beside the host's own on 22
  };
}
