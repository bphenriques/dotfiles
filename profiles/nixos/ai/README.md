# AI profile

Local Ollama on the laptop's GPU. Inference backend for Hermes Agent,
which runs on compute (not here).

- `services.ollama` listens on `0.0.0.0:11434` with
  `OLLAMA_CONTEXT_LENGTH=65536` so models default to a context window
  large enough for Hermes (which requires ≥64K).
- Firewall scoped by source IP (compute only) via `extraCommands`, not
  interface — survives WiFi/ethernet name changes.
- Pre-loaded models: `qwen2.5:7b` (primary, 128K native context),
  `qwen2.5vl:3b` (vision), `nomic-embed-text` (embeddings).

The design and rationale (brain on compute, surfaces, MCP tools, etc.)
live in `bphenriques-tools/agent-design-hermes-v10.md`.
