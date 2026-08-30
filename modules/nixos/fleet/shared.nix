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

    media.downloadCategories = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = "Download-client category per arr. The client writes completed downloads to <download-dir>/<category>, and the NAS precreates those directories.";
    };

    ai = {
      endpoint = {
        host = lib.mkOption {
          type = lib.types.str;
          description = "Address serving the fleet's OpenAI-compatible inference API. Consumers need only this and a model id, never the runtime behind it.";
        };
        port = lib.mkOption {
          type = lib.types.port;
          description = "Port the inference API listens on. Runtime-specific, so it moves with the endpoint.";
        };
      };
      model = lib.mkOption {
        type = lib.types.str;
        description = "Fleet-wide default model id, opaque and understood by the endpoint. Served as the Hermes default, the chat UI default.";
      };
      extraModels = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Additional model ids offered in the chat dropdown alongside the default (routed to the same endpoint).";
      };
    };
  };

  config.custom.fleet = fleet;
}
