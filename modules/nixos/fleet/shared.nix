{ config, lib, ... }:
let
  fleet = config.custom.fleet;
in
{
  options.custom.fleet = {
    ssh = {
      authorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "SSH public keys authorized across all hosts";
      };

      hostKeys = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
        description = "SSH host public keys, by hostname, for known-hosts pinning";
      };
    };

    dns = lib.mkOption {
      type = lib.types.str;
      description = "DNS resolver IP";
    };

    lan = {
      subnet = lib.mkOption {
        type = lib.types.str;
        description = "Main LAN subnet in CIDR notation";
      };

      hosts = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        description = "Static hostname to IP address mappings";
      };
    };

    # Guest hostname → bridge IP, for `ssh -J <host>` resolution + known-hosts pinning. Derived
    # from the microVM host's local allocation table; the rich placement stays host-local.
    microvmHosts = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "MicroVM guest hostname to its bridge IP";
    };
  };

  # Pin fleet host keys (name + its LAN/bridge IPs) so SSH verifies against the registry
  # rather than TOFU — stable across rebuilds, and a rotation is a loud registry edit.
  config.programs.ssh.knownHosts = lib.mapAttrs (name: publicKey: {
    hostNames = [ name ]
      ++ lib.optional (fleet.lan.hosts ? ${name}) fleet.lan.hosts.${name}
      ++ lib.optional (fleet.microvmHosts ? ${name}) fleet.microvmHosts.${name};
    inherit publicKey;
  }) fleet.ssh.hostKeys;
}
