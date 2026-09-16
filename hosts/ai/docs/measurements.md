# Measurements

Measured 2026-09-06 on `ai` (Ryzen AI Max+ 395, 128GB), Ollama 0.33.2-rocm, iGPU.
Numbers here are load-bearing. Do not re-derive them.

## Method

Reproduce with `tools/toolcall-bench.sh` (`-h` for options). Probes hit
`/v1/chat/completions`, the path hermes uses. Each request carries `SOUL.md`
as the system prompt plus 18 tool definitions matching the live `mcpServers` and
`platform_toolsets` surface. Three scenarios per trial:

- unambiguous tool with a checked argument (`get_current_time`, timezone)
- tool with a semantically close competitor present (`read_note` vs `list_notes`/`search_notes`)
- negative: a question that must produce no tool call

Context is inflated the way a real session inflates: a prior `fetch` turn whose tool
result carries repo text. Filler is real repo content, not synthetic.

## Model selection

| model                  | ctx~1.7k | ctx~12.8k | watch turn | decode   |
| ---------------------- | -------- | --------- | ---------- | -------- |
| qwen3.6:35b-a3b        | 44/45    | 45/45     | 1.46s      | 68 tok/s |
| qwen3.6:35b-a3b-coding | 45/45    | 45/45     | 0.95s      | 78 tok/s |
| qwen3.8:27b            | 45/45    | 44/45     | 3.75s      | 23 tok/s |
| gpt-oss:20b            | 38/45    | **22/45** | 1.33s      | 49 tok/s |

gpt-oss:20b was dropped on 2026-09-06. Its knee sits between ~4k and ~11k prompt tokens.
Past it, the characteristic failure is not a missing call: it selects the right tool and
supplies an argument lifted from surrounding context instead of from the request. That
produces confident wrong actions, which is worse than refusing.

qwen3.8:27b is dense, so decode costs 2.9x qwen3.6's MoE (3B active of 35B). Equally
reliable, 2.6x slower. A higher version number is not an upgrade across architectures.

It is still the coding pick as of 2026-09-08. This harness scores tool-call reliability, not
code quality, so it says nothing about the axis that choice rests on, and the latency lands on
`omp` rather than on the assistant.

Prefill figures are prefix-cache warm, which is the realistic case for a fixed system
prompt. Cold first requests cost more.

## Ruled out as causes of the gpt-oss degradation

Each was tested, not assumed.

| hypothesis                        | result                                               |
| --------------------------------- | ---------------------------------------------------- |
| `reasoning_effort = "low"`        | default effort scores identically                    |
| synthetic filler artifact         | real repo content reproduces it                      |
| context truncation                | `/api/ps` confirms `ctx=65536` as configured         |
| server or ROCm build              | reproduces on stock Ollama; not a Lemonade defect    |
| `q8_0` KV cache + flash attention | f16 control ties at baseline and flips sign at depth |

The f16 control is the reason `OLLAMA_KV_CACHE_TYPE = "q8_0"` stays. Three models share
that cache and only gpt-oss degraded, at a depth where quantization error is still tiny.

## Tool count is not the variable to watch

qwen3.6:35b-a3b, N=15 per cell. The 40-tool set adds direct competitors for both probe
targets (`read_local_file` against `read_note`, `world_clock` and
`get_timezone_for_location` against `get_current_time`), since crowding rather than raw
count is what plausibly breaks selection.

| tools | prompt | shallow | ~15k ctx | ~29k ctx |
| ----- | ------ | ------- | -------- | -------- |
| 18    | 1732   | 45/45   | 45/45    |          |
| 20    | 1847   | 44/45   |          |          |
| 25    | 2148   | 45/45   |          |          |
| 40    | 3081   | 45/45   | 44/45    | 45/45    |

Doubling the tool surface costs ~61 prompt tokens per tool and no measurable accuracy,
and it does not compound with context depth. Add MCP servers freely. The one crowding
failure observed in 180 calls was `read_local_file` selected over `read_note`, so if a new
server duplicates an existing tool's purpose, that pair is where to look first.

This is a property of the model, not of the box. It was gpt-oss:20b that collapsed, and it
collapsed on depth, not on tool count.

## Thinking is not switchable from hermes

qwen3.6 reasons by default. On Ollama's native `/api/chat`, `think: false` cuts a trivial
query from 254 output tokens to 4. Over `/v1`, `think`, `reasoning_effort`, and
`chat_template_kwargs` are all silently ignored.

Consequence: there is no per-request thinking toggle through hermes, because the toggle
lives only on the native API. All reliability and latency numbers above were measured
with thinking on. Tool-call turns reason briefly (~74 tokens), so the cost is small on
the dominant path, and disabling it might cost reliability rather than buy latency.

## The NPU is slower than the iGPU here

Relevant to any future Lemonade or FastFlowLM evaluation. AMD's own published FLM numbers
for `gpt-oss-20b` on XDNA2, against this box measured:

|             | NPU via FLM   | this iGPU         |
| ----------- | ------------- | ----------------- |
| decode @1k  | 18.2 tok/s    | 49.5 tok/s        |
| decode @64k | 8.7 tok/s     | operating context |
| prefill     | 221-477 tok/s | ~1455-1810 tok/s  |

The NPU is ~50 TOPS on every XDNA2 part. What differs is the iGPU beside it: 8 CUs on the
laptops Lemonade targets, 40 here. NPU offload wins when the NPU is the largest compute
block in the package. It is not, here. FLM's figures are from a Kraken Point system, so
they would improve somewhat on this memory bandwidth, not by 4x.

## Thermals under sustained load

Two hours of continuous inference: GPU 67C at 87W and 2470MHz, CPU 75C (k10temp), load
~2.0. Zero throttling events. Thermal limits are ~95-100C in a ~120W envelope, so
inference benchmarks on this box are not thermally biased.

A 16h agent session ran hotter and still did not throttle: 77C at 96W, 79C peak. See
`agent-load.md`, which also accounts for where wall clock goes on that path.

## Those numbers only hold for directly declared tools

Every figure above was measured with the tools declared in the request. Hermes does not
always do that: `tools.tool_search` defaults to `auto` and, once the listing passes 5% of
context, hides the MCP tools behind a search-then-`tool_call` indirection. qwen3.6:35b-a3b
drives that badly, calling tools with their required arguments missing:

```
tool_call to 'mcp__vault__search_notes' is missing required argument(s): query.
The tool was NOT invoked.
```

Observed 2026-09-08 as the agent insisting the vault was unavailable, then failing the same
call repeatedly. `tools.tool_search.enabled = "off"` puts the model back on the path
benchmarked here. The cost is the full listing inlined: the hermes system prompt went from
5390 to 7824 tokens, against a 64K window.

A benchmark of tool selection says nothing about a runtime that rewrites how tools are
offered. Check which of the two a client uses before trusting the table above.
