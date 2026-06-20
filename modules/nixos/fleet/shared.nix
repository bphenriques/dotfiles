{ lib, ... }:
{
  options.custom.fleet = {
    authorizedSSHKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "SSH public keys authorized across all hosts";
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

    microvm = {
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
        type = lib.types.attrsOf lib.types.str;
        description = "Microvm guest hostname to bridge IP mappings";
      };
    };
  };
}
