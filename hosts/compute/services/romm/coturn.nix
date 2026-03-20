# Minimal STUN/TURN server for EmulatorJS netplay (WebRTC).
# Static credentials + restricted to LAN/VPN via interface-scoped firewall rules.
{ config, self, ... }:
let
  listenPort = 3478;
  minPort = 49152;
  maxPort = 49999;

  # Static TURN credentials (not secret — access is controlled by firewall)
  turnUser = "romm";
  turnPassword = "romm-netplay";
in
{
  custom.homelab.services.coturn = {
    metadata.description = "STUN/TURN Server";
    metadata.version = "4.9.0";
    metadata.homepage = "https://github.com/coturn/coturn";
    metadata.category = "Media";
    port = listenPort;
    ingress.enable = false;
    integrations.monitoring.enable = false;
  };

  services.coturn = {
    enable = true;
    listening-port = listenPort;
    lt-cred-mech = true;
    no-cli = true;
    no-tcp-relay = true;
    min-port = minPort;
    max-port = maxPort;
    extraConfig = ''
      no-multicast-peers
      no-loopback-peers
      user=${turnUser}:${turnPassword}

      # Only allow relaying to LAN/VPN peers
      allowed-peer-ip=10.100.0.0-10.100.0.255
      allowed-peer-ip=192.168.1.0-192.168.1.255
    '';
  };

  # Interface-scoped firewall: only LAN (bond0) and VPN (wg0), not WAN
  networking.firewall.interfaces = {
    bond0 = {
      allowedUDPPorts = [ listenPort ];
      allowedUDPPortRanges = [{ from = minPort; to = maxPort; }];
    };
    wg0 = {
      allowedUDPPorts = [ listenPort ];
      allowedUDPPortRanges = [{ from = minPort; to = maxPort; }];
    };
  };
}
