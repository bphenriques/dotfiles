# compute_q: gemma4:e2b on compute as the always-available Hermes backend.
# Reachable from personal-agent over the compute-microvm bridge; not exposed
# to the LAN (bridge is trusted, bond0 firewall stays closed).
#
# Sized for service-tier work — intent parsing, tool calls, light summaries.
# Architecture context lives in voice-memo-pipeline.md at the repo root.
{ ... }:
{
  custom.ai.ollama = {
    enable = true;
    listenAddress = "0.0.0.0";   # firewall on bond0 blocks LAN; bridge interface is trusted
    loadModels = [ "gemma4:e2b" ];
    # Keep the model resident forever — compute has ~20 GB headroom under
    # `throttled.slice` and the cold-load is 60–90 s on CPU. Latency wins.
    # Laptop keeps the default 2m because RAM there is contested by gaming.
    keepAlive = "-1";
  };

  # Pin to the shared throttled.slice (same as Immich) and bump CPUWeight so
  # the agent's user-facing latency wins under contention. The matching
  # `immich-machine-learning.serviceConfig.CPUWeight = lib.mkForce 50` lift
  # is deferred — apply once the pipeline is actually running inference under
  # load and you want the priority enforced.
  systemd.services.ollama.serviceConfig = {
    Slice = "throttled.slice";
    CPUWeight = 500;
  };
}
