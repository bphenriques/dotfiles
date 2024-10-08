# These servers are not built using Nix but I am specifying here as I refer them in my desktop hosts.
{
  # Synology
  home-nas = {
    hostname = "192.168.68.53";
    ssh = {
      user = "Bruno-Admin";
      port = 6188;
    };
  };

  # Running pi-hole
  pi-zero = {
    hostname = "192.168.68.68";
    ssh = {
      user = "pi";
      port = 22;
    };
  };
}