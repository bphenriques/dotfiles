{ config, ... }:
{
  networking.firewall.interfaces.bond0 = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  # Answer this host's own ingress names on the LAN so they resolve without reaching the public zone.
  services.dnsmasq = {
    enable = true;
    alwaysKeepRunning = true;
    resolveLocalQueries = false;  # resolved owns resolv.conf; dnsmasq reading or claiming it loops the two together
    settings = {
      no-resolv = true;
      server = [ config.fleet.dns ];
      listen-address = [ "127.0.0.1" config.fleet.lan.hosts.compute ];
      bind-dynamic = true;        # default mode binds the wildcard, which collides with resolved on 127.0.0.53
      no-hosts = true;            # /etc/hosts maps compute to 127.0.0.2, which must not reach other clients
      bogus-priv = true;          # no-hosts leaves these unanswerable here, so fail them rather than leak upstream
      domain-needed = true;
      address = map (h: "/${h}/${config.fleet.lan.hosts.compute}") config.selfhost.ingress.hosts;
      local = map (h: "/${h}/") config.selfhost.ingress.hosts;   # since 2.86 address= forwards non-A types; per-name, so _acme-challenge still resolves publicly
    };
  };

  # Only the zone reaches dnsmasq: routing everything here made resolved mark it unhealthy when its upstream died.
  services.resolved.settings.Resolve.Domains = [ "~${config.selfhost.ingress.domain}" ];
  networking.nameservers = [ "127.0.0.1" config.fleet.lan.gateway ];  # the router answers zone names if dnsmasq is down
}
