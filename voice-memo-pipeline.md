# Voice memo pipeline + agent inbox

Delivers the four UX paths that must work without depending on the
laptop being on: vault capture, reminders, calendar events, voice
memo capture + action.

A future `personal-agent-memory` repo will hold medium- and long-term
memory with its own promote/demote cadences. Out of scope here.

## Why

Two gaps in the existing setup:

1. **Audio capture without the watch.** The Pebble + Core Devices design
   assumed Wrist-AI as the surface. We need a path for "I took a voice
   memo on my phone normally."

2. **A backend that's always-on without being cloud.** Laptop is off
   most of the time. Cloud is *off the table* — anything sent to
   Anthropic / OpenAI includes context that may contain private
   information (SOUL.md persona, recent conversation, memory snippets,
   vault excerpts), and that's a privacy boundary we don't cross. See
   "Why not cloud" below. The async pipeline needs to *just work*
   without making that trade.

Phone-side STT erases gap 1. A second Hermes backend (`compute_q`) on
the always-on compute host erases gap 2 *for async work*. Interactive
chat surfaces still depend on the laptop being on; when it's off,
they hard-fail rather than degrade.

## The line: surface-specific routing, no chains

| Workload type | Examples | Backend |
|---|---|---|
| **Service tier** | Intent parsing, tool calls, light summarization, vault Q&A | Compute. `gemma4:e2b`. |
| **Synthesis tier** | Cross-memo synthesis, weekly summaries, drafting, code, multi-doc pattern extraction | Laptop. `gemma4:e4b`. |

Rule: **silent local-to-local fallback for async; hard-fail for
interactive when laptop is off.** Within the local trust domain
(compute ↔ laptop) the two backends are interchangeable on async paths
and the worker picks per memo. Interactive surfaces wait for laptop —
they don't degrade to the slow CPU model and they don't reach for
cloud.

### Routing per surface

| Surface | Default | Other models exposed |
|---|---|---|
| Voice memo (async) | `laptop_primary` if reachable, else `compute_q` (worker probes per memo) | n/a |
| NextChat (browser) | `laptop_primary` (hard-fail when off) | `compute_q` in dropdown for "slow but available" |
| `hermes chat` (terminal) | `laptop_primary` (hard-fail when off) | `compute_q` via `--provider compute_q --model gemma4:e2b` |
| Pebble watch (future) | `laptop_primary` (hard-fail when off) | n/a (voice memo path is the laptop-off fallback) |

Async surfaces silently use whichever local backend is up because the
user doesn't see the difference, only the eventual ntfy result.
Interactive surfaces hard-fail when laptop is off because the only
realistic alternatives (cloud, slow compute) are *worse* than a clear
error: cloud is a privacy boundary we don't cross, and CPU inference
on compute is too slow for a chat UX (see speed numbers below).

The escape hatch for "I need to interact with the agent and laptop is
off" is **don't interact — drop it in the inbox via voice memo.** The
async pipeline handles it.

NextChat config: `CUSTOM_MODELS = "-all,+gemma4:e4b,+compute_q/gemma4:e2b"`,
`DEFAULT_MODEL = "gemma4:e4b"`. The literal strings are what Hermes
routes by; the dropdown shows them as-is.

### Speed expectations

| Backend | Decode rate | First-call wall-clock | Realistic use |
|---|---|---|---|
| `laptop_primary` (gemma4:e4b on RTX 4060) | ~25–40 tok/s | Seconds | Interactive chat at the desk; voice memo when laptop is on (silent fallback path); anything quality-sensitive. |
| `compute_q` (gemma4:e2b on throttled N150 CPU, 150% quota) | ~1–3 tok/s decode | **~5–10 minutes per turn** | Async / batch only. Voice memo when laptop is off. Available in NextChat dropdown as a "slow but available" choice if you actively want to wait. |

**The compute number is dominated by prefill, not decode.** Each turn
re-prefills the system prompt (SOUL.md + MCP tool schemas for `fetch`,
`time`, `vault`, plus Hermes' prelude) before any tokens come out.
That's ~1500–2500 tokens of prompt to process at ~3–5 tok/s on the
N150, which is ~5–10 minutes of fixed cost *per turn* regardless of
how short the response is. The 150 % CPU quota is a deliberate thermal
cap (the N150 has previously hit shutdown temperatures under sustained
load), so lifting it isn't an option. There's no cleverness around
this — it's the hardware floor.

For async surfaces, that's fine: voice memo → record → walk away →
ntfy notification minutes later is exactly the UX the pipeline is
built for. For interactive surfaces, that's *not* fine, and the
routing rule above reflects that.

### What compute_q is actually good at

Even within "async only," not all queries are equal for the smaller
model. The real distinction is **single-step intent-and-act** vs
**multi-step retrieval-and-synthesis**:

| Pattern | Compute_q (gemma4:e2b) | Notes |
|---|---|---|
| **Single tool call + confirmation.** "Remind me at 3pm to call John" → `cronjob` (built-in) → "✓ set for 15:00." | ✓ Good | One round-trip, one tool, no synthesis. Quality is fine. |
| **Pure capture, no tool call.** Voice memo → "filed under inbox/<id>." | ✓ Good | Summary quality is mediocre, but the raw transcript is preserved alongside it, so the user has fallback. |
| **Calendar add from natural language.** "Lunch with Sarah Tuesday at noon" → `calendar.add_event` → "✓ added." | ✓ Good | Same shape as reminders. |
| **Multi-step retrieval + synthesis.** "What did I write about restructuring recipes?" → vault search → vault read × N → synthesize. | ✗ Poor | Each tool call is a full round-trip (~25 min on this hardware); 2 tool calls = ~60 min observed. Model also lacks the meta-reasoning to filter false-positive results (test fixtures matched by keyword search, name-matched files for the wrong "partner"). |
| **Drafting / writing.** "Help me write a polite reply to this email." | ✗ Poor | Quality drop is too large at e2b scale. |

The line is: **compute can call a tool. compute struggles to reason
about whether the tool's result was useful and whether to call more
tools.** For voice memos that map directly to a tool call (reminder,
calendar, single capture), compute is the right size. For anything
requiring "search → judge results → maybe search again → synthesize,"
compute either fails outright or burns an hour to produce a low-quality
answer.

Implication for the inbox-worker: the cleanest v1 behaviour is to
**not register the vault MCP server on the `compute_q` path**. Vault
Q&A queries that arrive when laptop is off should either wait for
laptop (synthesis-tier path) or come back with a "vault Q&A needs the
desktop; query archived for later" response — not a slow, wrong answer
generated on compute. The architecture for this is discussed in
"Per-model tool scoping" below.

### Why not cloud

Cloud (Anthropic / OpenAI) would be the obvious answer for "make
interactive chat work when laptop is off." It's deliberately *not* the
answer.

Anything sent to a cloud LLM includes the active context: SOUL.md
(persona, geography, role), recent conversation history, memory
snippets the agent might have surfaced, vault excerpts that the agent
chose to read into the turn. Any of those can contain personal or
private information that the user has no audit trail of after it
leaves the homelab. The cloud provider's retention and re-training
policies are theirs, not ours.

A narrower "cloud for *some* turns, not others" carveout doesn't
help: by the time the agent decides which tool to call or what to
pull from the vault, the system prompt has already shipped, and the
mistake — sending something that should have stayed local — happens
at a layer the user can't intervene at. The simpler boundary is "no
cloud at all" until we have a meaningfully different design (e.g.,
the agent splits cleanly into a local "context-aware" half and a
remote "context-free" half — a different doc, deferred).

The cost of this principle is real: when laptop is off, *interactive*
surfaces hard-fail. The user either waits for the laptop or drops the
query into the async path (voice memo). That's the trade.

[`hermes-cloud-bridge.md`](./hermes-cloud-bridge.md) captures the
earlier exploration of how a cloud opt-in *might* work; it's left in
place as architectural reference but is not part of the current plan.

### Per-model tool scoping

We want `compute_q` to *not* have the `vault` MCP server registered —
both to keep prefill smaller (~400 tokens saved per turn) and to
prevent the model from trying multi-step vault Q&A it isn't good at.

Hermes doesn't natively support per-model tool scoping. `mcpServers`
is a global config; `platform_toolsets` is keyed by surface (`cli`,
`api_server`), not by model. The client passing an OpenAI-style
`tools` list in the request is ignored (Hermes uses its registered
MCPs). So there's no clean knob to flip.

The options, ranked by ROI:

1. **Worker-side prompt injection (cheap, unreliable).** When the
   inbox-worker calls Hermes with `model=compute_q`, prepend a system
   message: "Tools restricted: `vault_*` unavailable. If the query
   requires vault access, respond with `DEFER_TO_LAPTOP` and stop."
   Worker greps for the sentinel, archives the entry as "deferred,
   needs laptop." Soft restriction; small models sometimes ignore.
2. **Two Hermes instances (clean, heavy).** VM runs two `hermes
   gateway` processes on different ports: one with the full toolset
   (default = `laptop_primary`, port 8642), one with vault excluded
   (default = `compute_q`, port 8643). NextChat dropdown chooses by
   URL; inbox-worker picks URL based on laptop probe. ~2× hermes
   memory + activation overhead. Cleanest from a routing-correctness
   standpoint; meaningful operational cost.
3. **Worker-side classification before Hermes (medium).** Inbox-worker
   runs a rules-based or tiny-LLM-based classifier on the transcript;
   vault-tagged queries get archived with `defer_for: laptop` instead
   of routed to compute. Doesn't help NextChat / chat surfaces.
4. **Accept the limitation (zero cost).** Document that compute does
   vault Q&A badly; rely on user self-discipline to not ask vault
   questions when laptop is off; bad answers if they do.

Going with **#1 for v1** because it's reversible and visible (you'll
see the `DEFER_TO_LAPTOP` archive entries piling up if the heuristic
is failing). Escalate to **#2** if vault Q&A traffic on compute proves
non-trivial. **#3** is only worth it if we have a synthesis-tier
laptop wake job to drain the deferred queue, which is a separate
build.

## Phone-side STT

The phone records, transcribes locally, and shares the **text** (not
audio) to the ntfy `personal-agent-inbox` topic. Nothing audio leaves
the phone; the inbox carries text.

| Platform | Recommended (OSS leaning) | Trade-off |
|---|---|---|
| **iOS** | [Aiko](https://apps.apple.com/app/aiko/id1672085276) (free) — uses [WhisperKit](https://github.com/argmaxinc/WhisperKit) (MIT) + OpenAI Whisper models (MIT). App shell is closed. | OSS stack underneath; closed wrapper. Practically equivalent privacy to Apple's built-in. |
| **iOS** (alternative) | Apple Voice Memos with built-in transcription. | Closed end-to-end. Best UX, zero install. |
| **Android** | WhisperBoard / whisper.cpp-based apps. | Fully OSS. Quality excellent for English. |
| **Pixel** | Recorder. | Best quality; closed weights (Gemini Nano). |

Practical limitation: Whisper's English transcription is consistently
excellent; mixed-language and noisy audio degrade. If quality slips
below useful, the answer is a larger Whisper variant (small/medium runs
on modern phones with NPU acceleration), not abandoning on-device STT.

## Two repos, two trust levels

| Repo | Owner | Writes | Branch protection |
|---|---|---|---|
| **`bphenriques/notes`** (existing vault) | bphenriques | Laptop synthesis-tier jobs open PRs using `bphenriques`'s own SSH key; bphenriques merges | `main` protected: only bphenriques |
| **`bphenriques/personal-agent-inbox`** (new) | personal-agent | The VM's `inbox-worker.service` pushes directly to `main` using the VM's existing `personal-agent` SSH key | none |

The vault stays user-curated knowledge; the inbox is the agent's
working set. Two distinct trust scopes, encoded as gitea collaborator +
branch protection.

**Important constraints**:

- **Compute does not write to gitea.** Compute hosts gitea (server) and
  hosts Ollama for `compute_q` (inference). It does not run agent
  logic, does not hold any repo clone, does not need a gitea identity.
- **The VM's existing `personal-agent` SSH key handles inbox writes.**
  Same key that authenticates SSH login, sops decryption, and the
  read-only vault clone. One key, four jobs — no new identity to
  provision.
- **The vault is only ever written from the laptop side.** Synthesis-tier
  cron jobs read the inbox repo via gitea, generate vault-shape notes,
  push branches to `bphenriques/notes` using bphenriques's own key.
  Laptop never talks to the VM; laptop talks to gitea.

### `personal-agent-inbox` layout

```
inbox/
  <id>/
    note.md      # frontmatter + body (original text + agent response inline)
archive/
  <YYYY>/<MM>/<id>/
```

`<id> = <unix-ts>-<sha256(text)|head -c 8>` — deterministic from input.
Same text yields same id; ntfy delivering the same message twice (rare,
but possible on subscriber restart) doesn't double-process.

`note.md` frontmatter, v1 minimum:
```yaml
---
source: ntfy-inbox                    # ntfy-inbox | scheduled-summary | webhook | …
captured_at: 2026-05-31T14:23:00+01:00
backend_used: laptop_primary          # laptop_primary | compute_q — which Hermes backend the worker chose
---

# Captured text

<original text from phone>

# Agent response

<Hermes' synthesized reply, including any tool-call results>
```

The worker always archives after one Hermes call (v1). If downstream
automation later needs structured classification (e.g., `reminder` vs
`capture`) or upsert refs, add a JSON-tag prompt instruction so Hermes
emits them at the end of the response, and grow the frontmatter then.
v2 polish, not v1.

## Ingestion: phone → ntfy text → inbox-worker (on the VM)

Phone records, transcribes locally, shares the text to the ntfy
`personal-agent-inbox` topic. The subscriber runs **on the VM** (not on
compute).

Two subscriber paths exist:

- **v0 — Hermes' upstream ntfy platform adapter** (`v2026.5.29.2+`)
  subscribes directly. Messages flow phone → ntfy → adapter → Hermes
  chat pipeline → reply published to `personal-agent`. End-to-end for
  the reminders subset (`cronjob` is a built-in agent tool). No custom
  worker.
- **v1 — `inbox-worker.service`** (custom, future). For the full
  voice-memo pipeline (id derivation, repo commit, archive), a `ntfy
  subscribe --cmd` worker on the VM owns the message and calls Hermes
  itself, gaining control over backend selection, classification, and
  the gitea push. The adapter would be moved to a dedicated topic
  (e.g. `personal-agent-chat`) so only chat-style pushes hit it.

The worker lives with Hermes (on the VM) because the agent owns this
workload. Compute is the service-provider (gitea, ntfy, Ollama for
`compute_q`); the VM is the agent runtime + the agent's data. The
worker uses the VM's existing SSH key for the inbox repo push;
compute's gitea write scope stays empty.

Two topics, paired:

| Topic | Direction | Publisher | Subscriber | Auth |
|---|---|---|---|---|
| `personal-agent-inbox` | phone → ntfy → VM | phone (token-auth, write-only) | VM's Hermes ntfy adapter (v0) / `inbox-worker.service` (v1) — both connect over the bridge to compute's ntfy | private; topic + ACL provisioned via `custom.homelab.services.hermes-api.integrations.ntfy` (`ro` on inbox) |
| `personal-agent` | VM → ntfy → phone | VM's Hermes (cron / agent replies) publishes via the bridge | phone (subscribed on device) | same publisher entry (`wo` on `personal-agent`); single ntfy user, single token |

(Considered SMB and Syncthing; ntfy wins on auth + retry + no-new-app
+ no Synology setup.)

### inbox-worker pipeline

```
phone (record + STT locally)
  └─ share text to ntfy ─────► compute: ntfy server (private topic)
     (via system share sheet)         │
                                      ▼ (bridge)
                            VM: inbox-worker.service (ntfy --cmd handler)
                                      │
                                      ▼
                            ack → ntfy(personal-agent): "📥 received: <preview>"
                                  (via bridge, published from VM)
                                      │
                                      ▼
                            probe http://laptop:11434/api/tags
                              if reachable → model = laptop_primary
                              else          → model = compute_q
                                      │
                                      ▼
                            Hermes (local on VM) with service-tier tools:
                              • intent parse
                              • optional tool calls:
                                - cronjob                 (Hermes built-in; reminders fire on schedule
                                                           and deliver via the ntfy home channel)
                                - calendar.add_event      (MCP, CalDAV → Radicale on compute; future)
                                - vault-mcp read for Q&A  (MCP, local vault clone on VM)
                              • Ollama backend call:
                                - laptop_primary → laptop:11434 (LAN)
                                - compute_q     → compute:11434 (via bridge)
                              • synthesize response
                                      │
                                      ▼
                            commit note.md to personal-agent-inbox repo;
                            git mv inbox/<id> → archive/<yyyy>/<mm>/<id>;
                            git push to gitea on compute (via bridge,
                              VM's own SSH key)
                                      │
                                      ▼
                            done → ntfy(personal-agent):
                                  "✅ <agent response>"

phone subscribes to personal-agent topic ◄────────────── (ack + result both arrive here)
```

### Observability via ntfy

Because `compute_q` takes seconds-to-minutes per memo, the user needs
to know work is happening. The pattern:

- **Ack** (sub-second after ntfy receipt): `📥 received: <first 60 chars of memo>`. Tells the user the homelab heard them.
- **Done** (when work completes): `✅ <classification>: <agent's reply>`. Includes any tool-call confirmations ("reminder set for 15:00") or PR-suggestion links.
- **Failure** (if anything errored): `❌ <reason>: queued for retry`. The frontmatter stays `action_taken: false`; the next worker tick retries from where it stopped.

The user sees two notifications per memo on the happy path. If the
work takes >30 s, an optional progress nudge can be inserted — but the
ack already tells them the homelab is working, so an additional
"processing…" message is usually noise. Skip it for v1.

## Hosts and what they own

```
Compute (always-on host)        VM (agent runtime)              Laptop (when on)
────────────────────────        ──────────────────              ─────────────────
• ntfy server                   • hermes-agent                  • Ollama (laptop_primary)
• gitea (server)                  - ntfy platform adapter       • synthesis-tier cron jobs
• Ollama (compute_q)              - cronjob built-in              (read inbox repo via
• Radicale (calendar)             - Honcho memory (local)         bphenriques's key,
• immich, jellyfin              • Calendar MCP (future)           write vault PRs)
                                  (CalDAV → compute's Radicale)
                                • Vault clone (read-only,
                                  for vault MCP)
                                • inbox-worker.service (v1)
                                  + personal-agent-inbox clone
                                  (read-write, agent identity)
```

**Compute is the service-provider, not an agent.** It runs gitea
(server), ntfy (server), Ollama for `compute_q` (inference). It does
not run agent logic, does not write to any repo, does not need a gitea
client identity. Its `throttled.slice` budget is just for inference.

**The VM is the agent runtime + the agent's data.** Hermes, MCP
servers, and worker live here. The VM's existing `personal-agent` SSH
key handles SSH login, sops decryption, vault read-clone, and inbox
write-clone — one identity, four jobs.

**Laptop is a synthesis-tier processor.** When it's on, cron-driven
hermes-skill jobs read `personal-agent-inbox` via gitea, generate
cross-memo summaries / weekly themes / vault-shape note drafts, and
push branches to `bphenriques/notes` using **bphenriques's own SSH
key** (already authorised on the vault). Laptop never talks to the VM;
laptop talks to gitea. Idempotent skills handle the "missed last
week's run because laptop was off" case.

The inbox repo is the queue between layers — the VM writes raw
captures, laptop reads them later for deeper synthesis.

**The line on time-sensitivity**: if the user needs sub-minute
responsiveness (e.g., "remind me in 30 seconds"), they use the phone's
native reminders/timer — not this pipeline. The pipeline owns
*minute-to-hour* time scales. "Remind me at 3pm" two hours from now
works fine even if processing takes a couple of minutes; the reminder
still lands at 3pm.

## Hermes' role: one call per memo, drive the workflow

**v1**: the worker calls Hermes once per memo with **all service-tier
tools registered** (reminders, calendar, vault MCP read). Hermes parses
intent, optionally calls tools, and synthesizes a text response. The
worker stores the response verbatim in `note.md` body and archives
unconditionally. No classification parsing; the user reads the note
body to see what happened.

The OpenAI-compatible response returns the synthesized assistant
message, not the orchestration log. There's no clean machine-readable
"which tool got called" signal from a single chat-completion request,
so v1 doesn't try.

**v2 (if real use demands it)**: prompt-engineer Hermes to end every
response with a one-line JSON tag (`{"action": "reminder", "ref":
"<id>"}`). Worker greps the tag, populates additional frontmatter
fields, downstream automation can link entries by ref. Defer until you
have a concrete need — e.g., a synthesis-tier job that wants to know
which inbox entries triggered reminders.

The point of going through Hermes per memo is not classification; it's
to register intent + invoke tools + produce a user-readable
confirmation. The classification is a side-effect of *whether tools
got called*, but the user doesn't need a machine-tagged label for
that — they see the response and know.

## Idempotency

Every operation must be safe to retry; the worker is restartable
mid-anything.

- **Inbox id**: deterministic from input — `<unix-ts>-<sha256(text)|head -c 8>`. Replays yield the same id.
- **Inbox commit**: target path is `archive/<yyyy>/<mm>/<id>/` (v1 archives unconditionally after one Hermes call). If the path exists, worker skips.
- **Reminder creation**: tool takes `external_ref = "<id>"`; upserts on conflict.
- **Calendar event**: CalDAV `UID = "<id>@personal-agent"`; upserts on conflict.
- **Backend choice (`laptop_primary` vs `compute_q`)**: doesn't affect `<id>`. The same memo replayed with laptop on then off would produce the same id and skip the second time.

State is implicit (path exists ⟹ done). Crash recovery is "next ntfy
delivery; worker sees the path, skips" — no processing dir, no recovery
sweep, no stale locks.

## Compute resource budget

Single inference component lives in the existing `throttled.slice`
(cores 1-2, CPUQuota 150 %, MemoryHigh 16 GB, MemoryMax 20 GB), shared
with Immich and Jellyfin.

| Component | Working set | Notes |
|---|---|---|
| gemma4:e2b (Q4 + 64K KV cache q8) | ~5 GB | Steady during inference, ~5–8 tok/s under throttled.slice |
| **Total active** | ~5 GB | ~11 GB headroom to MemoryHigh |

Room to step up to `gemma4:e4b` on compute if service-tier quality on
async paths becomes a complaint. Native function calling, 128K context,
same family as laptop's primary — identical tool-call schema and
prompt template.

**Priority over Immich**: under contention Ollama wins.

```nix
# host config, sketch
systemd.services.ollama.serviceConfig.CPUWeight = 500;
systemd.services.immich-machine-learning.serviceConfig.CPUWeight = lib.mkForce 50;
```

`CPUWeight` (default 100) is proportional: 500 vs 50 means Ollama gets
~10× more CPU under contention. Memory budget stays shared in the
slice; systemd-oomd resolves pressure (already enabled). A dedicated
`ai-inference.slice` with its own MemoryHigh is deferred unless
evidence demands it.

## Trust boundaries

| Component | Location | Trust |
|---|---|---|
| inbox-worker.service (v1) | VM | Trusted, runs as the same user that owns the persistent volume |
| Hermes-agent | VM | Trusted; cron state in `$HERMES_HOME`, persists across VM rebuilds via /var/lib/hermes volume |
| Hermes ntfy adapter | VM, single token, two ACLs | Token-scoped (`wo` out + `ro` inbox); plain-text bridge IP, trusted interface |
| Calendar MCP (future) | VM, CalDAV client only | Trusted, stateless (Radicale on compute holds state) |
| compute_q Ollama | compute, throttled.slice, 127.0.0.1 | Trusted, no agent logic |
| ntfy topics (in + out) | compute ntfy server, private + token | Token-scoped, per-direction |
| personal-agent-inbox writes | gitea on compute, VM's `personal-agent` SSH key | VM can write inbox only; **no compute gitea identity** |
| Vault PR | gitea, `main` protected | Only laptop writes (synthesis-tier jobs, bphenriques key); user merges |

The ntfy message body is untrusted user input but only ever passes
through Hermes (a text-completion model) and gets written to git. No
code execution path.

## Phased rollout

Each phase is independently useful — you could stop after any and
have something working.

1. **ntfy topics + Hermes ntfy adapter on the VM.** Topics
   `personal-agent-inbox` (in) and `personal-agent` (out) declared via
   `custom.homelab.services.hermes-api.integrations.ntfy` (single
   publisher entry: `wo` on `personal-agent` + `extraAccess.personal-agent-inbox = "ro"`).
   Compute's ntfy-configure generates one token; one-time sops mirror
   into the VM as `hermes-agent/ntfy-publisher-token`. The Hermes
   adapter (upstream plugin, `v2026.5.29.2+`) subscribes via
   `NTFY_TOPIC` and publishes via `NTFY_HOME_CHANNEL`. Reminders flow
   end-to-end at this phase via the built-in `cronjob` tool — no
   custom worker, no MCP build.

   *Status: shipped.* Smoke test: `hermes cron create "1m" "test"
   --no-agent` → phone buzzes.

2. **`compute_q` Ollama on compute.**
   `custom.ai.ollama.enable = true` on compute with `gemma4:e2b`,
   `keepAlive = "-1"`, `CPUWeight = 500`. Hermes registers the
   backend via `settings.custom_providers`. Verify
   `hermes chat --provider compute_q --model gemma4:e2b -q "hi"` from
   the VM.

   *Status: shipped.*

3. **NextChat dropdown** exposes both local backends: `gemma4:e4b`
   (default, hard-fails when laptop is off) and `compute_q/gemma4:e2b`
   (slow but always available). No cloud entry.

   *Status: shipped.*

4. **Calendar MCP on the VM** (off-the-shelf CalDAV client → Radicale
   on compute via bridge). `UID = <id>@personal-agent` upsert. Adds a
   second always-on intent target alongside `cronjob`.

5. **`inbox-worker.service` for the full voice-memo capture path.**
   Custom worker on the VM owns the inbox topic (Hermes adapter moves
   to `personal-agent-chat` for ad-hoc Q&A). Derives `<id>`, writes
   `/var/lib/hermes/inbox/<id>/note.md`, calls Hermes with
   service-tier tools, commits + pushes to the
   `personal-agent-inbox` gitea repo via the VM's existing SSH key,
   publishes "done" to `personal-agent`. Backend selection happens
   per-memo (probe laptop, fall to `compute_q`).

Synthesis-tier laptop skills (cross-memo summaries, vault-PR
composition) come *after* this pipeline is stable — they consume from
the inbox repo and produce vault PRs.

## Open questions to revisit after deploy

1. **Memo title generation.** The `<id>` is timestamp + hash; the
   inbox entry needs a human-readable name somewhere. Hermes can
   generate one as part of its response. Decide from first batch of
   real memos.

2. **Laptop probe timeout tuning.** Default ~1 s. If laptop is on but
   slow to respond (waking from suspend, cold-starting Ollama), worker
   might silently fall to `compute_q` when laptop was actually about
   to come up. Acceptable for v1; if it bites, bump the timeout or
   add a "warmup" call before the probe.

3. **Hermes response parsing for v2.** v1 stores the response
   verbatim in `note.md`; the user reads it. If automated downstream
   processing needs structured signal (linking reminder entries to
   inbox provenance, etc.), add a JSON-tag prompt instruction so
   Hermes emits the tag and the worker greps it.

4. **Memo titles vs slug-as-title.** The branch / archive path uses
   `<id>` (machine slug). A human-friendly title goes in the note
   body / commit message. Probably enough; if you find yourself
   needing to search by title in the gitea UI, add it to the path.

## Out of scope

- Cloud routing. Rejected on privacy grounds — see "Why not cloud" above. `hermes-cloud-bridge.md` is left in place as architectural reference, not as a planned step.
- Hardware refresh. The N150's prefill speed is the real ceiling for compute-tier interactive UX. A future host with more memory bandwidth (or a discrete GPU) would meaningfully change the math; until then, compute is async-only.
- Pebble Wrist-AI wiring (`agent-design-hermes-v10.md`, deferred until the watch is in hand).
- Synthesis-tier laptop jobs (cross-memo summaries, weekly themes, vault PR composition) — get their own design pass after the immediate pipeline is stable.
- Auto-PR creation against `bphenriques/notes` from the inbox-worker path — handled by laptop synthesis-tier jobs only (using bphenriques's own SSH key). The VM never writes to `notes`.
- Web app recording surface — phone-side STT removed the need; revisit only if real use surfaces friction with the ntfy share-sheet flow.
- Dedicated `ai-inference.slice` for memory partitioning — `throttled.slice` budget has 11 GB headroom.
- **Long-term memory** (`personal-agent-memory`, future) — promote/demote cadences over distilled knowledge.
