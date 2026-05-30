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
          description = "Bridge interface name on the microvm host (also matched as iifname in nftables).";
        };
        gateway = lib.mkOption {
          type = lib.types.str;
          description = "Host's IP on the microvm bridge — the gateway for guests.";
        };
        prefixLength = lib.mkOption {
          type = lib.types.int;
          description = "Prefix length of the microvm bridge subnet.";
        };
      };

      hosts = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        description = "Microvm guest hostname to IP address mappings.";
      };
    };

    ai = {
      model = lib.mkOption {
        type = lib.types.str;
        description = ''
          Fleet-wide default LLM model identifier (Ollama format, e.g. `gemma4:e4b`).
          Pulled by the Ollama host, served as `model.default` to the Hermes runtime,
          and advertised by the NextChat UI. Keeping it fleet-level avoids drift.
        '';
      };
      contextLength = lib.mkOption {
        type = lib.types.ints.positive;
        default = 65536;
        description = ''
          Token context window shared between Ollama (`OLLAMA_CONTEXT_LENGTH`)
          and Hermes (`model.context_length`, `auxiliary.compression.context_length`).
          Must agree across the stack — Hermes enforces a 64K minimum and rejects
          requests where the upstream advertises less.
        '';
      };
      maxTokens = lib.mkOption {
        type = lib.types.ints.positive;
        default = 8192;
        description = "Maximum output tokens per Hermes response.";
      };
    };
  };
}
