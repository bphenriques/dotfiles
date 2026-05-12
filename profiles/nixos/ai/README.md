# AI profile

Local Ollama + OpenWebUI, plus a pull-mode worker pattern: compute (always-on)
enqueues jobs into SMB; this laptop drains them when awake.

## Decisions

- **Pull, not push.** Laptop is not always-on. No WoL, no network exposure.
- **Per-task systemd unit.** Per-task sandbox scoping (`ReadWritePaths`,
  `RestrictAddressFamilies`, etc.). Extract `mkAgentWorker` into
  `agents/lib.nix` on the **second** task — not the first.
- **Ollama-only for v1.** Privacy first; laptop is a stepping stone before
  hardware investment. `OLLAMA_KEEP_ALIVE=2m` because 8 GB VRAM is shared
  with PRIME-sync desktop. Anthropic API as fallback brain is a future
  option for tasks where Qwen3:8b tool use is unreliable.
- **Goose CLI** as agent runtime (MCP-native, headless). OpenWebUI stays
  as the chat surface; Goose Desktop unused.
- **Task definition lives on SMB, not in this repo.** This profile ships
  the runtime; the SMB share ships `AGENTS.md` + `.agent/` scripts.
  Iteration requires no rebuild.

## Skill-style task layout

Each task is a self-contained folder on its SMB share — a manifest plus
arbitrary scripts, in the spirit of Claude Code Skills:

```
<task-share>/
├── AGENTS.md          # task instructions and format rules
├── .agent/
│   ├── fetch          # input acquisition (often source-aware)
│   ├── run            # entry point: validate + publish
│   └── ...            # whatever else this task needs
└── <outputs>
```

The contents of `.agent/` are **not** standardized across tasks. Variation
points differ (recipes by source, statements by issuer); don't impose
fixed verbs like `plan/execute/validate`.

## Worker contract

Constant across tasks; the nix worker enforces it:

1. Worker creates a per-job `staging_dir` under `PrivateTmp`.
2. Goose runs the recipe with `staging_dir` + `<task>_dir` + input. The
   agent populates `staging_dir`. Goose's exit code is informational.
3. **Worker** (not the LLM) calls `<task>_dir/.agent/run "$staging_dir"`.
   Its exit code is the success signal.
4. Failure: worker preserves `staging_dir` and `source.txt` into
   `<inbox>/../failed/<job>/`. Goose+run output is in the journal.

Splitting the publish step out of the LLM loop is a security boundary:
the LLM cannot be prompt-injected into skipping validation or directing
the publish with weird args. `.agent/run` is the only code path that
writes into `<task>_dir`.

## Agent's blast radius

Per task, defense-in-depth:

- Systemd: `DynamicUser`, `ProtectSystem=strict`, `ReadWritePaths` scoped
  to inbox + task dir, `RuntimeMaxSec` cap, restricted address families.
- `BindReadOnlyPaths` on `<task>_dir/AGENTS.md` and `<task>_dir/.agent`
  so the agent cannot rewrite its own instructions or replace the
  validator. Caveat: bound with leading `-`, so on the very first run
  after boot — before SMB automount has triggered — the bind may be
  skipped for that one invocation.
- Publish step runs outside the LLM loop (see worker contract).
- `.agent/run` owns slug validation, no-overwrite enforcement, and
  atomic moves into `<task>_dir`.

## Current tasks

- [`recipe-to-cooklang`](./agents/recipe-to-cooklang/) — URL → cooklang.
  Inbox in `bphenriques`, output in `media/recipes`. Source dispatch
  (youtube/instagram/web) lives inside `.agent/fetch`.
