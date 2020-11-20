{
  # Using Google DNS but should consider something else...
  networking = {
    knownNetworkServices = ["Wi-Fi"]; # List available ones with `networksetup -listallnetworkservices`
    dns = ["8.8.8.8" "8.8.4.4" "2001:4860:4860::8888" "2001:4860:4860::8844"];
  };
}
