{ lib, fleet, ... }:
{
  options.custom.fleet = {
    ssh = {
      authorizedKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        description = "SSH public keys authorized across all hosts";
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

    microvms = lib.mkOption {
      type = lib.types.attrsOf (lib.types.attrsOf lib.types.str);
      default = { };
      description = "MicroVM host to its { guest hostname -> bridge IP } table";
    };

    ai = {
      model = lib.mkOption {
        type = lib.types.str;
        description = "Fleet-wide default LLM tag (Ollama format). Pulled by the Ollama host, served as the Hermes default, the chat UI default.";
      };
      extraModels = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Additional Ollama tags pulled and offered in the chat dropdown alongside the default (routed to the same Ollama backend).";
      };
    };
  };

  config.custom.fleet = fleet;
}
