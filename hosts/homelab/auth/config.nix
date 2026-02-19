{ config, pkgs, lib, self, ... }:
let
  networking = import ../networking.nix;
in
{
  imports = [
    ./hardware.nix
    ./microvm.nix
    ../../../config/common.nix
    ../../../config/nixos/headless/default.nix
    ./services
  ];

  # Network configuration
  networking = {
    hostName = "auth";
    nameservers = networking.cloudflare.nameservers;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 2222 80 443 ];  # SSH (internal only), HTTP/HTTPS (filtered below)
      allowedUDPPorts = [ 443 ];          # QUIC/HTTP3

      # Restrict HTTPS (443) to Cloudflare IPs + internal subnet. Ensures origin IP protection even if leaked.
      # These rules run BEFORE the generic allow rules, so we drop non-matching traffic first.
      extraInputRules = let
        cloudflareIPv4Set = "{ ${builtins.concatStringsSep ", " networking.cloudflare.ipv4} }";
        cloudflareIPv6Set = "{ ${builtins.concatStringsSep ", " networking.cloudflare.ipv6} }";
      in ''
        # Allow 443 from internal subnet
        ip saddr ${networking.bridge.subnet} tcp dport 443 accept
        ip saddr ${networking.bridge.subnet} udp dport 443 accept

        # Allow 443 from Cloudflare IPs
        ip saddr ${cloudflareIPv4Set} tcp dport 443 accept
        ip saddr ${cloudflareIPv4Set} udp dport 443 accept
        ip6 saddr ${cloudflareIPv6Set} tcp dport 443 accept
        ip6 saddr ${cloudflareIPv6Set} udp dport 443 accept

        # Drop 443 from all other sources (before allowedTCPPorts processes it)
        tcp dport 443 drop
        udp dport 443 drop
      '';
    };
  };

  # SSH on non-standard port (internal access only)
  services.openssh.ports = [ 2222 ];

  # Secrets - age key auto-generated on first boot, stored in persistent /var
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age = {
    keyFile = "/var/lib/sops-nix/key.txt";
    generateKey = true;   # Auto-generate if missing
    sshKeyPaths = [];     # Don't use SSH host keys (ephemeral in microvm)
  };

  # Users - root access via SSH key for management
  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3"
  ];

  system.stateVersion = "25.11";
}
