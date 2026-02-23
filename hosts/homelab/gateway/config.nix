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
    hostName = "gateway";
    nameservers = networking.cloudflare.nameservers;
    firewall = {
      enable = true;
      # No ports opened by default - all handled by explicit rules
      allowedTCPPorts = [ ];
      allowedUDPPorts = [ 51820 ];  # WireGuard for Gerbil tunnels

      # All rules here run BEFORE allowedTCPPorts processing
      extraInputRules = let
        cloudflareIPv4Set = "{ ${builtins.concatStringsSep ", " networking.cloudflare.ipv4} }";
        cloudflareIPv6Set = "{ ${builtins.concatStringsSep ", " networking.cloudflare.ipv6} }";
      in ''
        # SSH (2222) only from internal subnet
        ip saddr ${networking.bridge.subnet} tcp dport 2222 accept
        tcp dport 2222 drop

        # HTTPS (443) from Cloudflare + internal subnet only
        # No HTTP (80) - SSL only
        ip saddr ${networking.bridge.subnet} tcp dport 443 accept
        ip saddr ${networking.bridge.subnet} udp dport 443 accept
        ip saddr ${cloudflareIPv4Set} tcp dport 443 accept
        ip saddr ${cloudflareIPv4Set} udp dport 443 accept
        ip6 saddr ${cloudflareIPv6Set} tcp dport 443 accept
        ip6 saddr ${cloudflareIPv6Set} udp dport 443 accept
        tcp dport 443 drop
        udp dport 443 drop

        # Block HTTP entirely (SSL only)
        tcp dport 80 drop
      '';
    };
  };

  # SSH on non-standard port (hardening handled in config/nixos/headless)
  services.openssh.ports = [ 2222 ];

  # Secrets - age key auto-generated on first boot, stored in persistent /var
  sops.defaultSopsFile = ./secrets.yaml;
  sops.age = {
    keyFile = "/var/lib/sops-nix/key.txt";
    generateKey = true;
    sshKeyPaths = [];
  };

  # Users - root access via SSH key for management
  users.mutableUsers = false;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3"
  ];

  system.stateVersion = "25.11";
}
