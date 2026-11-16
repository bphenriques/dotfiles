{ lib, ... }:
let
  endpoint = description: lib.mkOption {
    inherit description;
    type = lib.types.submodule {
      options = {
        host = lib.mkOption { type = lib.types.str; };
        port = lib.mkOption { type = lib.types.port; };
      };
    };
  };
in
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
      endpoint = endpoint "OpenAI-compatible inference API.";
      speechEndpoint = endpoint "Speech-to-text API (whisper.cpp server).";
      assistantEndpoint = endpoint "Assistant's OpenAI-compatible API.";
      model = lib.mkOption {
        type = lib.types.str;
        description = "Default model id for the assistant, the chat UI and coding agents.";
      };
      uncensoredModel = lib.mkOption {
        type = lib.types.str;
        description = "Uncensored model id.";
      };
      imageModel = lib.mkOption {
        type = lib.types.str;
        description = "Model id for image generation and editing.";
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

    forge.sshEndpoint = endpoint "Forge's git over SSH.";
  };
}
