# AI Host: Parked

Nothing here is agreed.

## Raise the load end of the fan curve

[`fan-curve.nix`](hardware/fan-curve.nix) rewrites only the two records below 45C. Everything from 55C
up is factory, and factory never exceeds **32%**, at any temperature:

| Temp                      | 55C | 65C | 75C | 85C | 90C | above |
| ------------------------- | --- | --- | --- | --- | --- | ----- |
| Ours (= factory)          | 22% | 23% | 25% | 28% | 32% | 32%   |
| `silenzioso`, interpolated | 15% | 22% | 31% | 55% | 75% | 100%  |

The last record holds for everything above it, so there is no band that ever spins the fans harder
than 32%. Above 90C the chip's only defence is to throttle. Every published community profile for this
board, including the ultra-quiet one, reaches 100% by 94-97C.

**The catch that makes this worth considering:** the quiet community profiles are *quieter than factory*
below ~65C and only ramp above ~80C. So this is not a noise-for-cooling trade. It is strictly better on
both axes, if the premise holds.

**Measure before changing anything, because the premise may not hold here.** The community complaint is
sustained LLM inference parking the APU near Tjmax. Our own measurement on this box was **CPU 60C, GPU
48C at 104W PPT, sclk 2900MHz, no throttling** (gpt-oss:20b, 47 tok/s). If a long run never gets near
80C, this whole item is moot. The test is a sustained generation while watching `k10temp` and `sclk` for
a clock drop.

### If it does need doing

Duty-only is the cheap option and needs no new mechanism: extend the existing write to the duty byte of
records 3 to 7 and leave every temperature byte alone. Hysteresis stays correct for free, since each
record's step-down byte is the previous record's temperature and none of those move.

The alternative, adopting a community profile verbatim, means writing temperature and hysteresis bytes
too, because their trip points differ (`silenzioso` is 50/62/72/80/86/92/97 against factory
55/65/75/85/90). More code, more to get wrong, and the interpolated version above captures the shape.

Do not write `0x26-0x28` (fan 1) or `0x46-0x48` (fan 2). Those are firmware-managed and volatile.

## Inference runtime

Measured 2026-10-25 on Ollama 0.40.2-rocm. Short prompts are a 300-word story (prose) and a Python module with
tests (code); the long prompt is 10K tokens.

| Model                         | Prose tok/s | Code tok/s | Prefill tok/s @10K | MTP acceptance        |
| ----------------------------- | ----------- | ---------- | ------------------ | --------------------- |
| `qwen3.6:35b-a3b` (`ai.model`) | 64-67       | 83         | 1102               | 43-48% prose, 82% code |
| `qwen3.8:27b` (`codingModel`)  | 15-16       | 24-29      | 277-334            | 28-33% prose, 76% code |

- 0.40.12 (2026-10-29), `qwen3.6:35b-a3b`: decode unchanged within noise, prefill @10K 1292-1297 (+17%).
  At depth: 1021 pp / 51 tg @36K, 839 pp / 39 tg @62K.
- ROCm works: gfx1151 detected, every layer offloaded, flash attention on. The "Ollama ignores ROCm" reputation
  does not apply here.
- Ollama already runs MTP speculative decoding on both models (`draft-mtp` in the journal). The `-mtp` library
  tags add nothing; 0.40.11 measured the same.
- The dense 27B decodes at the ~256 GB/s bandwidth wall when MTP misses (prose). No runtime fixes that.
- Measure with `/api/generate`, `think:false`, and read `prompt_eval_*` / `eval_*` from the response.

### Candidates

**Qwen3.8-Flash-Next** is the Qwen3.8 MoE: 125B with 6B active, plus a 51B n-gram table and a 4B MTP head
([unsloth GGUF](https://huggingface.co/unsloth/Qwen3.8-Flash-Next-GGUF)). Not in the Ollama library. Q4_K_XL is
111 GB against a 112 GiB `ttm.pages_limit`, so it only fits with ComfyUI stopped and nothing else resident.
On llama.cpp it collapses with depth, which is what Ollama would get
([Gufo's measurements](https://github.com/gufo-org/gufo/blob/main/docs/models/qwen3.8-flash-next/BENCHMARKS.md)):

| Depth | Gufo pp | llama.cpp pp | Gufo tg (MTP, mixed) | llama.cpp tg (MTP, mixed) |
| ----- | ------- | ------------ | -------------------- | ------------------------- |
| 0     | 1735    | 469          | 36                   | 32                        |
| 32K   | 1619    | 278          | 40                   | 25                        |
| 64K   | 1556    | 208          | 39                   | 20                        |
| 128K  | 1481    | 135          | 40                   | 15                        |

**[Gufo](https://github.com/gufo-org/gufo)**: gfx1151-only engine with per-model HIP kernels, MIT, Nix flake and
podman image. The real wins are prefill (27B: ~650 vs 357 pp) and Flash-Next at depth. Its 27B "70 tok/s"
headline is repetitive text; mixed text is ~27. Rejected for now: a second server that does not coordinate
memory with Ollama means remembering to unload one, and the project is two months old with no releases, three
text models and no embeddings. As of 2026-10-29 it cuts near-daily releases (v0.23 to v0.27 in a week), serves
`/embeddings` (EmbeddingGemma 2) and moved to ROCm 10.1. Flash-Next peaks at ~89-91 GiB, so it still cannot sit
beside the 22 GB `ai.model` under the 112 GiB limit.

**[Halogen](https://github.com/peonist-ai/halogen-flash-server)**: Peonist's engine for Flash-Next only, gfx1151
only, podman image, OpenAI and Anthropic APIs on one port. Proprietary binary under an EULA. Vendor numbers:
~1700 pp flat to 128K, 54 tok/s prose, 73 code. [Level1Techs](https://forum.level1techs.com/t/ryzen-ai-halo-halogen-server-testing-notes/257455)
reproduced them (1695 pp @32K, ~45-53 tok/s, 45 at 258K) and measured ~77 GiB at 256K context, which would leave
room for `ai.model`. Its docs still ask for a dedicated machine, it pins memory, and startup can fail with HIP OOM
when the KV pool is too large. Same coordination problem as Gufo, plus closed source.

**llama.cpp**: `qwen4exp` (Flash-Next) MTP merged 2026-10-01 ([#29761](https://github.com/ggml-org/llama.cpp/pull/29761)).
Mainline work is active: QSA selection without the kq_mask (-4 GiB, #30965, 10-23), DFlash drafter inputs
(#30927, 10-22); an MTP startup abort has a fix pending (#30860). Gufo's comparison uses llama.cpp from
2026-09-22, so it predates all of this. Ollama runs `qwen4exp` via `ollama create` from a GGUF (a collaborator
confirmed on 0.40.6, [#18817](https://github.com/ollama/ollama/issues/18817)) but has no library tag; its own
Flash-Next work is MLX-only ([#18078](https://github.com/ollama/ollama/pull/18078)). No stated timeline.
Measuring Flash-Next imported into Ollama is the cheapest next test: if it holds up, it stays one server.

Measured 2026-10-29, Ollama 0.40.13-rocm (llama.cpp b11963), unsloth UD-IQ4_XS, same prompts as above.
The unsloth root MTP head is byte-identical to ggml-org's.

| Flash-Next variant  | Prose tok/s | Code tok/s | pp @10K | pp / tg @36K | pp / tg @62K | Acceptance prose / code | GTT |
| ------------------- | ----------- | ---------- | ------- | ------------ | ------------ | ----------------------- | --- |
| No speculation      | 22.5        | 22.6       | 486     | 409 / 16.7   | 351 / 13.4   |                         | 62G |
| `DRAFT` ggml-org MTP | 27-28       | 46         | 465     | 391 / 27.3   | 328 / 25.0   | 36-39% / 86%            | 66G |
| `DRAFT` DFlash       | 27          | 48         | 477     | 400 / 26.9   | 342 / 18.6   | 29-30% / 75%            | 64G |

- Mainline caught up on depth: tg holds 25 @62K with MTP (Gufo's September llama.cpp run: 20 @64K), pp 328 (was 208).
- Prefill is the blocker: about a third of `qwen3.6:35b-a3b` (486 vs 1295 @10K), a quarter of Gufo and Halogen.
- Fits beside the pinned `ai.model` (64-66G resident vs 22G), unlike Gufo.
- Against `codingModel` (`qwen3.8:27b`): faster decode on code (46 vs 24-29) and prefill (486 vs 277-334).
  Against `qwen3.6:35b-a3b`: slower on everything. Quality is the open question.
- Import: `FROM /models/Qwen3.8-Flash-Next-UD-IQ4_XS-*.gguf`, `DRAFT <mtp gguf>`, `PARAMETER draft_num_predict 3`.

**[Lemonade](https://phoronix.com/review/lemonade-vulkan-rocm)**: AMD's server over llama.cpp (ROCm or Vulkan),
FLM for the NPU, and the experimental HRX ROCm backend (30-50% prefill claimed, one Qwen3 MoE so far).
Same llama.cpp underneath, so expect Ollama-like numbers; the appeal is maturity and one front end managing
backends.

Swapping is count-based, not memory-budgeted ([multi-model](https://github.com/lemonade-sdk/lemonade/blob/main/docs/guide/configuration/multi-model.md)):

- `max_loaded_models` is an LRU slot count per model type (LLM, embedding, ...), default 1. Pinning exists.
- A failed load (e.g. out of memory) evicts every model and retries once. That is what would make room for a big
  model, after a failed first attempt.
- `auto_evict` (opt-in) polls GPU usage every 2s and unloads the most disposable idle model above a threshold
  (default 90%). On an APU it reads GTT and counts other processes, so ComfyUI would push it out. Reactive only:
  nothing checks fit before a load, and a model mid-request is never evicted.
- No Gufo backend, so Lemonade cannot serve Flash-Next any faster than Ollama would.

Remaining questions:

- Does it run llama.cpp's MTP for qwen35/qwen35moe, or would it regress the numbers above?
- Embeddings (`embeddinggemma`) and the OpenAI endpoint hermes and the chat UI already use.
- HRX prefill on qwen3.6:35b-a3b once it covers more than one model.

### Quality

Only data point is [one 10-task coding benchmark on this chip](https://www.soothill.io/blog/2026/08/14/coding-model-benchmark-strix-halo/):
gpt-oss-120b 9/10 at 50 tok/s, Qwen3.8-27B + MTP 8/10 at 22-25, qwen3.6-35B-A3B 6/10 at 60. Flash-Next untested
there; its scores are vendor-only.
