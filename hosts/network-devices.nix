# These servers are not built using Nix but I am specifying here as I refer them in my desktop hosts.
{
  # Server: Synology
  home-nas = {
    hostname = "192.168.68.53";
    ssh.user = "Bruno-Admin";
    ssh.port = 6188;
  };

  # Server: pi-hole
  pi-zero = {
    hostname = "192.168.68.68";
    ssh.user = "pi";
    ssh.port = 22;
  };

  # Device: Retrohandeld
  rg353m = {
    hostname = "192.168.68.63";
    ssh.user = "ark";
    ssh.port = 22;
  };

  # Device: Steam Deck
  deck = {
    hostname = "192.168.68.67";
    ssh.user = "deck";
    ssh.port = 22;
  };
}
