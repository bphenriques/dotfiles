# Tailscale

Assumptions:
- The machine hosting tailscale server has a static IP.
- The DNS record of the domain points to the IP mentioned before.

## Account Setup

1. Disable SSH access. I rather have that disabled, and it likely won't work as my servers use a different default port.
2. Enable device approval.

## Tailscale Server

1. Add the `192.168.1.192/32`.
2. Enable `Exit Node`. Also enable exit node local network access.
3. Approve the requests in the admin console.

## Access Control

Have this policy in-place that only grants access to my homelab:
```json
{
  "grants": [
    // Homelab services behind HTTPS port
    {
      "src": ["autogroup:member"],
      "dst": ["192.168.1.192"],
      "ip":  ["tcp:443"],
    },
    // Syncthing file sharing. Access to specific folders is controlled by the service
    {
      "src": ["autogroup:member"],
      "dst": ["192.168.1.192"],
      "ip":  ["tcp:22000", "udp:22000"],
    },
  ],
}
```