# AI profile

Local Ollama on the laptop's GPU. Inference backend for the homelab
Hermes Agent (which runs on the `personal-agent` microvm hosted on
compute, not here).

- `services.ollama` listens on `0.0.0.0:11434` with
  `OLLAMA_CONTEXT_LENGTH=65536` (Hermes requires ≥64K).
- Firewall scoped by source IP (compute only) via `extraCommands`, not
  interface — survives WiFi/ethernet name changes.
- Pre-loaded models: `gemma4:e4b` (primary, via `config.custom.fleet.ai.model`),
  `qwen2.5vl:3b` (vision), `nomic-embed-text` (embeddings).
