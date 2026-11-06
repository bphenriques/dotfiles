{ lib, ... }:
{
  options.fleet = {
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

      gateway = lib.mkOption {
        type = lib.types.str;
        description = "LAN gateway (the router)";
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

    ups = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "UPS name as upsd serves it.";
      };

      host = lib.mkOption {
        type = lib.types.str;
        description = "Address of the machine running upsd.";
      };
    };

    downloadCategories = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      description = "Download-client category per arr.";
    };

    notifyTopics = lib.mkOption {
      type = lib.types.attrsOf lib.types.bool;
      description = "Notification topics, as topic -> readable without authentication.";
    };

    ai = {
      endpoint = {
        host = lib.mkOption {
          type = lib.types.str;
          description = "Address serving the OpenAI-compatible inference API.";
        };
        port = lib.mkOption {
          type = lib.types.port;
          description = "Port the inference API listens on.";
        };
      };
      imageEndpoint = {
        host = lib.mkOption {
          type = lib.types.str;
          description = "Address serving image generation and editing.";
        };
        port = lib.mkOption {
          type = lib.types.port;
          description = "Port the image API listens on.";
        };
      };
      speechEndpoint = {
        host = lib.mkOption {
          type = lib.types.str;
          description = "Address serving speech-to-text (whisper.cpp server).";
        };
        port = lib.mkOption {
          type = lib.types.port;
          description = "Port the speech-to-text API listens on.";
        };
      };
      assistantEndpoint = {
        host = lib.mkOption {
          type = lib.types.str;
          description = "Address serving the assistant's OpenAI-compatible API.";
        };
        port = lib.mkOption {
          type = lib.types.port;
          description = "Port the assistant API listens on.";
        };
      };
      model = lib.mkOption {
        type = lib.types.str;
        description = "Default model id for the assistant and the chat UI.";
      };
      uncensoredModel = lib.mkOption {
        type = lib.types.str;
        description = "Uncensored model id.";
      };
      codingModel = lib.mkOption {
        type = lib.types.str;
        description = "Default model id for terminal coding agents.";
      };
      embeddingModel = lib.mkOption {
        type = lib.types.str;
        description = "Model id for document embeddings.";
      };
      contextLength = lib.mkOption {
        type = lib.types.ints.positive;
        description = "Context window the endpoint serves.";
      };
    };

    forge.sshEndpoint = {
      host = lib.mkOption {
        type = lib.types.str;
        description = "Address serving the forge's git over SSH.";
      };
      port = lib.mkOption {
        type = lib.types.port;
        description = "Port the forge's SSH listens on.";
      };
    };
  };
}
