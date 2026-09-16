# Agent load

Observed 2026-09-08 on `ai`, Ollama 0.33.2-rocm, iGPU. Not a benchmark: a 16h session of
the laptop's `omp` coding agent hitting `qwen3.8:27b` (`fleet.ai.codingModel`) over
`/v1/responses`, with `qwen3.6:35b-a3b` resident and idle beside it. Complements
`measurements.md`, which scores tool-call reliability. This one accounts for wall clock.

Nothing here was acted on. See [Parked](#parked).

## Where the 11-15 minutes go

Requests ran 11-15 min each. Decode dominates, so wall clock tracks output tokens, not
prompt size:

| task  | prefill         | decode              | total    |
| ----- | --------------- | ------------------- | -------- |
| 11640 | 9.3s / 1543 tok | **411s** / 4812 tok | ~7 min   |
| 10589 | 77s / 17636 tok | **156s** / 1855 tok | ~3.9 min |
| 11417 | 1.5s / 190 tok  | 57s / 1024 tok      | ~1 min   |

Decode held 11.7-12.0 tok/s (85 ms/tok) at 22K-45K context depth. Prefill was 166-230 tok/s
at depth, 368-626 tok/s when partially cache-warm. The 23 tok/s in `measurements.md` was
measured shallow; at agent depth it is half that.

**Only two levers exist: fewer output tokens, or faster ones.** Prompt engineering that
trims the system prompt moves nothing on this path.

## 12 tok/s is the ceiling, not a fault

Dense 27.3B at Q4_K_M is ~17GB resident. At ~256 GB/s that caps decode near 15 tok/s, so
12 is ~80% of theoretical. The box is doing what the README's bandwidth model predicts and
cannot go meaningfully faster on this model.

The idle `qwen3.6:35b-a3b` does 68 tok/s (`measurements.md`; 78 for the `-coding` variant)
because it touches 3B active of 35B. The spread on the same hardware, same session, is ~6x.

## Thermals need no action

Sustained across the full 16h, sampled at 5s for 60s plus Prometheus range queries:

|                | sustained | 16h max |
| -------------- | --------- | ------- |
| GPU edge       | 76-77C    | 79C     |
| CPU Tctl       | 77C       | 79.75C  |
| GPU PPT        | 96W       | 129.91W |
| NVMe Composite | 37.9C     | 42.9C   |

`sclk` sat at 2452-2462MHz (DPM 1 of 600/2479/2900), `mclk` pinned at its 1000MHz max,
`gpu_busy_percent` at 100%, GTT 44 of 112 GiB. **Zero** throttle, thermal or amdgpu-error
entries in `journalctl -k` over the whole uptime.

Hotter than the 67C/87W two-hour figure in `measurements.md` and still ~20C off the ~95-100C
limit, with 34W of the envelope unused. Cooling is not a constraint on this workload.

100% busy at mid-DPM clock with power to spare is the signature of a bandwidth-bound
kernel, not a throttled one.

## Both models are hybrid attention

Confirmed from the runtime, not inferred from model cards. One full-attention layer in four:

| model             | arch        | n_layer | KV layers | KV @64K (q8_0) |
| ----------------- | ----------- | ------- | --------- | -------------- |
| `qwen3.6:35b-a3b` | `qwen35moe` | 40      | 10        | 680 MiB        |
| `qwen3.8:27b`     | `qwen35`    | 64      | 16        | 2176 MiB       |

Consequence: recurrent state cannot be rewound, so a diverged prompt prefix forces a full
cold re-prefill rather than a partial one. Caught live at 22528 tokens with
`n_prompt_tokens_cache = 0`, roughly 110s before the first output token. It fired 20 times
in 16h, so it is a real but minority cost, and `prompt_save` writes 1.3-1.8 GiB state files
to the NVMe that also holds the OS.

**SWA is ruled out as the cause.** `n_swa = 0` and `is_swa_any = 0` on both. The runtime log
line naming it is a generic hint:

```
forcing full prompt re-processing due to lack of cache data (likely due to SWA or
hybrid/recurrent memory, see .../pull/13194#issuecomment-2868343055)
```

Do not chase `--swa-full`. There is no flag for the hybrid branch; it is architectural.

## Speculative decoding is earning its keep

`--spec-type draft-mtp` with a 1-layer f16 draft KV (128 MiB MoE, 256 MiB dense). Acceptance
0.45-0.68, mean accepted length 2.36-3.04, per-position accept `(0.72, 0.54, 0.42, 0.33)`.
The 12 tok/s above is already with this help. Leave it on.

## Levers identified, not applied

Ranked by expected effect on wall clock. None measured, all cheap and reversible.

1. **Thinking budget and temperature.** `ollama show qwen3.8:27b` reports a baked-in
   `temperature 1` and a `thinking` capability, and the live slot confirms 1.0 in use.
   Output tokens are the wall clock at 85 ms/tok, so this is the largest lever and the only
   one that needs no redeploy. Qwen suggests ~0.6 for thinking mode. Note the constraint in
   `measurements.md`: over `/v1`, `think` and `reasoning_effort` are silently ignored, so
   the toggle has to come from the client or the native API.
2. **`OLLAMA_KV_CACHE_TYPE = "f16"`.** Costs +2.1 GiB (dense) and +0.66 GiB (MoE) against
   68 GiB of free GTT, and drops q8_0 dequantization out of every attention op at depth. The
   comment at `services/ollama.nix:35` justifies q8_0 by VRAM saving, which this box does not
   need. **Read the f16 control in `measurements.md` first**: f16 ties at baseline and flips
   sign at depth, and that control is why q8_0 was chosen. Latency and reliability may want
   opposite settings here.
3. **Revisit the dense-for-quality trade.** `hosts/shared.nix:48` picks `qwen3.8:27b` for
   code quality. That rests on a leaderboard score, since `measurements.md` scores tool
   calls (45/45 for both) and explicitly disclaims the code-quality axis. The price is now
   known: ~6x, or a 1-minute response becoming 7.

## Ruled out

| hypothesis                                       | result                                                               |
| ------------------------------------------------ | -------------------------------------------------------------------- |
| thermal or power throttling                      | no kernel events in 16h, 20C and 34W of headroom                     |
| SWA breaking the prefix cache                    | `n_swa = 0`, `is_swa_any = 0` on both models                         |
| forcing `power_dpm_force_performance_level=high` | decode is bandwidth-bound; buys watts, not tokens                    |
| request queueing                                 | `-np 1`, but one client serialized by choice, never observed waiting |

## Unrelated bug

One request returned 500 after 4m36s:

```
mtmd_helper_bitmap_init_from_buf: failed to decode buffer as either image or audio
(video support not compiled in)
```

Both models load with `--mmproj` and advertise `vision`, so something client-side attached
media the runtime could not decode. Independent of load.

## Parked

Deliberately not acted on. `qwen35`/`qwen35moe` are new architectures still maturing on
gfx1151, and the hybrid-attention KV path is exactly the part in motion upstream. Tuning
against it now risks encoding workarounds for defects that get fixed.

Re-measure against the numbers above when the Ollama or ROCm pin next moves. The cheapest
signals that something changed: decode above ~12 tok/s at 30K depth, or the
`forcing full prompt re-processing` count falling.

## What is missing from monitoring

No `fan*_input` under any hwmon node, and `sensors` returns nothing, so fan RPM is invisible.
Temps are the only proxy for a failing fan. Not urgent at 77C, but there would be no early
warning.
