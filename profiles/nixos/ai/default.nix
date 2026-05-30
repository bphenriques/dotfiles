# Laptop's role in the AI stack: host the local LLM. gemma4:e4b runs ~9.6 GB Q4 —
# mostly fits an RTX 4060 8 GB with q8_0 KV cache; small CPU spill keeps it snappy
# (~15–22 t/s). Sized for note-taking / calendar / fetch / summarisation, not deep
# code reasoning. Step up to gemma4:26b if that workload grows.
{ config, ... }:
{
  custom.ai.ollama = {
    enable = true;
    listenAddress = "0.0.0.0";
    loadModels = [
      config.custom.fleet.ai.model
      "qwen2.5vl:3b"
      "nomic-embed-text"
    ];
    # Compute reaches Ollama over LAN. Compute is the homelab gateway;
    # the laptop is never directly exposed externally.
    allowFromHosts = [ config.custom.fleet.lan.hosts.compute ];
  };
}
