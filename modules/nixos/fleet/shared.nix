{ config, lib, ... }:
let
  fleet = config.custom.fleet;

  microvmHostOpt = lib.types.submodule {
    options = {
      ip = lib.mkOption {
        type = lib.types.str;
        description = "Mapped IP to the bridge";
      };

      mac = lib.mkOption {
        type = lib.types.str;
        description = "MAC Address";
      };

      vsockCid = lib.mkOption {
        type = lib.types.int;           # FIXED: Changed from attrsOf int to just int
        description = "ID of vsocket id for systemd notification (readiness)";
      };
    };
  };
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

    computeMicrovm = {
      bridge = {
        name = lib.mkOption {
          type = lib.types.str;
          description = "Bridge interface name on the microvm host";
        };
        gateway = lib.mkOption {
          type = lib.types.str;
          description = "Host's IP on the microvm bridge";
        };
        prefixLength = lib.mkOption {
          type = lib.types.int;
          description = "Bridge subnet prefix length";
        };
      };

      hosts = lib.mkOption {
        type = lib.types.attrsOf microvmHostOpt;
        description = "Microvm guest hostname to its settings";
      };
    };
  };

  # Pin fleet host keys (name + its LAN/bridge IPs) so SSH verifies against the registry
  # rather than TOFU — stable across rebuilds, and a rotation is a loud registry edit.
  config.programs.ssh.knownHosts = lib.mapAttrs (name: publicKey: {
    hostNames = [ name ]
      ++ lib.optional (fleet.lan.hosts ? ${name}) fleet.lan.hosts.${name}
      ++ lib.optional (fleet.computeMicrovm.hosts ? ${name}) fleet.computeMicrovm.hosts.${name}.ip;
    inherit publicKey;
  }) fleet.ssh.hostKeys;
}
