{ config, pkgs, ... }:
{
  # Ollama — local LLM inference with CUDA + CPU hybrid offload.
  # 64K context required by Hermes; KV cache q8_0 halves VRAM on RTX 4060 8 GB.
  # gemma4:e4b chosen as primary — Gemma 4 family's MatFormer "edge" variant,
  # ~4B active params per token. 9.6 GB Q4 mostly fits on GPU (small spill to
  # RAM), so throughput stays snappy (~15–22 t/s). Native function calling,
  # 128K context. Sized for note-taking / calendar / fetch / summarisation —
  # not for deep code reasoning. Step up to gemma4:26b if that workload grows.
  services.ollama = {
    enable = true;
    package = pkgs.ollama-cuda;
    loadModels = [ "gemma4:e4b" "qwen2.5vl:3b" "nomic-embed-text" ];
    host = "0.0.0.0";
    environmentVariables = {
      OLLAMA_KEEP_ALIVE = "2m";
      OLLAMA_MAX_LOADED_MODELS = "1";
      OLLAMA_NUM_PARALLEL = "1";
      OLLAMA_FLASH_ATTENTION = "1";
      OLLAMA_KV_CACHE_TYPE = "q8_0";
      # Hermes Agent enforces a 64K minimum context. Anything lower is
      # rejected at request time ("below the minimum 64,000 required").
      # Keep this at 65536 — lowering it for VRAM headroom is a non-starter.
      OLLAMA_CONTEXT_LENGTH = "65536";
    };
  };

  # Compute reaches Ollama over LAN. Compute is the homelab gateway;
  # the laptop is never directly exposed externally. Source-IP scoped so
  # it survives WiFi/ethernet swaps (interface names change; the IP doesn't).
  # Uses iptables backend (NixOS default). If the host ever flips to
  # `networking.nftables.enable = true`, convert to `extraInputRules`.
  networking.firewall.extraCommands = ''
    iptables -I nixos-fw -p tcp -s ${config.custom.fleet.lan.hosts.compute} --dport 11434 -j nixos-fw-accept
  '';
}
