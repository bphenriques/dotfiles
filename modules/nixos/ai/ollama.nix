# Local LLM inference via Ollama. Opt-in; hosts that don't enable it pay only
# the option-declaration cost.
{ config, lib, pkgs, ... }:
let
  cfg = config.custom.ai.ollama;
in
{
  options.custom.ai.ollama = {
    enable = lib.mkEnableOption "Ollama local LLM inference";
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.ollama-cuda;
      defaultText = lib.literalExpression "pkgs.ollama-cuda";
      description = "Ollama package; switch to pkgs.ollama for CPU-only hosts.";
    };
    listenAddress = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "Bind address for the Ollama HTTP server.";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 11434;
    };
    loadModels = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Models pre-pulled at activation. Include `config.custom.fleet.ai.model` for consistency with the rest of the stack.";
    };
    allowFromHosts = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Source IPs allowed through the firewall to the Ollama port (iptables backend).";
    };
  };

  config = lib.mkIf cfg.enable {
    services.ollama = {
      enable = true;
      package = cfg.package;
      host = cfg.listenAddress;
      port = cfg.port;
      loadModels = cfg.loadModels;
      environmentVariables = {
        OLLAMA_KEEP_ALIVE = "2m";
        OLLAMA_MAX_LOADED_MODELS = "1";
        OLLAMA_NUM_PARALLEL = "1";
        OLLAMA_FLASH_ATTENTION = "1";
        OLLAMA_KV_CACHE_TYPE = "q8_0";  # halves VRAM on 8GB cards
        OLLAMA_CONTEXT_LENGTH = toString config.custom.fleet.ai.contextLength;
      };
    };

    # Source-IP-scoped firewall opens. iptables backend (NixOS default);
    # flip to extraInputRules if a host enables nftables.
    networking.firewall.extraCommands = lib.mkIf (cfg.allowFromHosts != [ ]) (
      lib.concatMapStringsSep "\n" (ip:
        "iptables -I nixos-fw -p tcp -s ${ip} --dport ${toString cfg.port} -j nixos-fw-accept"
      ) cfg.allowFromHosts
    );
  };
}
