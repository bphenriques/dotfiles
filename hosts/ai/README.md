# AI Host

Design for the NixOS host that owns local inference. Not built yet; hardware pending. Phased on
purpose: this is a learning project in a fast-moving space, so phase 1 is deliberately small and
everything else is listed but not designed.

Today the fleet's model runs on `laptop` (RTX 4060, 8GB), which caps the assistant at a 4B model and
only answers while the laptop is awake. This box moves inference onto always-on hardware with 128GB
of unified memory. hermes-agent stays in `agent-vm` on compute: only the endpoint moves.

## Use case

**A personal assistant with strong MCP tooling.** Everything below follows from that, and it is worth
being explicit because it inverts the obvious metric:

- **Tool-call reliability is the success metric, not tokens/second.** An assistant that generates at
  100 t/s but emits a malformed `tool_call` one time in five is useless. Published figures for local
  models sit around 90% well-formed calls on simple workloads and 80-90% end-to-end on multi-step
  ones, and that spread is dominated by the model and the server's chat-template handling, not by the
  hardware.
- **Time-to-first-token is the latency you feel**, not generation rate. An agent loop is many short
  turns with large prompts (system prompt plus tool schemas plus prior tool results), so prefill
  dominates.
- **Context headroom is a tool-calling requirement.** Tool schemas and results fill a window fast, and
  a truncated window breaks the loop quietly rather than loudly. hermes' 64K is the floor, not a
  luxury.

So: pick the model for its trained-in function calling, pick the runtime for its tool-call parsing
fidelity, and treat throughput as a constraint to satisfy rather than a number to maximise.

## Hardware

Minisforum MS-S1 MAX:

- AMD Ryzen AI Max+ 395 (Strix Halo), 16 Zen 5 cores / 32 threads
- Radeon 8060S iGPU, 40 RDNA 3.5 CUs, `gfx1151`
- XDNA 2 NPU (see [NPU](#npu) below, it is no longer a dead end on Linux)
- 128GB LPDDR5X-8000, soldered, 256-bit
- 2x M.2: one PCIe 4.0 x4, one x1
- Dual 10GbE, USB4 v2, PCIe x16 slot
- Internal 320W PSU, ~160W peak / ~130W sustained

**Memory bandwidth is the ceiling, not compute.** Token generation on a dense model is bounded by
reading the weights once per token, so `t/s ≈ bandwidth / model bytes`. At ~256 GB/s a 70B model at
q4 (~40GB) tops out around 6 t/s no matter what backend runs. MoE models only touch their active
experts per token, which is why gpt-oss-120b at MXFP4 (~60-65GB) generates ~55 t/s while a dense 70B
crawls. **Prefer MoE.** The 128GB buys model *size*, not model *speed*.

**The second M.2 is x1**, roughly 2 GB/s against roughly 7 GB/s real-world on the x4 slot. A 60GB
model loads in ~30s versus ~9s. Models live on the x4 device.

### NPU

The XDNA 2 NPU is usable on Linux as of Lemonade 10.0 (March 2026), via the FastFlowLM runtime. This
is a recent development and it inverts the previous "AMD NPU is Windows-only" answer.

What makes it relevant here rather than a curiosity: **the NPU does prefill while the iGPU does token
generation**, which cuts time-to-first-token roughly in half. Per the use case above, TTFT is the
latency an agent loop actually exposes to you. This is the one accelerator story on this box that
targets the metric that matters.

Requirements, all satisfiable:

- `amdxdna` kernel driver. Already present in the fleet's kernel: `CONFIG_DRM_ACCEL_AMDXDNA=m` is set
  in nixpkgs' 7.1 series (verified on 7.1.5), so no DKMS.
- NPU firmware 1.1.0.0 or later.
- **AMD IOMMU enabled.** Disabling it makes the NPU invisible. Worth stating loudly because the
  Strix Halo GTT tuning guides tend to focus on memory knobs and skip this.

Caveat worth knowing before leaning on it: FastFlowLM's orchestration is MIT, but the NPU kernels
themselves are prebuilt binaries rather than source. Free for use, including commercially, and the
team has since joined AMD. If binary blobs in the inference path are a problem, this is where to
notice it.

Treat the NPU as a phase-2 experiment, not a phase-1 dependency.

## What NixOS owns and what containers own

**Containers are the right call for the runtime, and NixOS is still the right call for the host.**
These are not in tension, and the split is not a compromise:

| NixOS owns (declared, reproducible)            | Containers own (fast-moving, disposable) |
| ---------------------------------------------- | ---------------------------------------- |
| Kernel, `amdgpu`, `amdxdna`, firmware          | Inference server and its engines         |
| BIOS-adjacent kernel params (IOMMU, GTT)       | ROCm / Vulkan userspace                  |
| Users, firewall, SSH, node-exporter            | Model weights and their cache            |
| The container *declaration* and its pinned tag |                                          |

Concretely: AMD's Lemonade is not in nixpkgs at all (`nixpkgs#lemonade` is an unrelated clipboard
tool from 2021). Meanwhile llama.cpp and the ROCm stack for `gfx1151` iterate weekly and ship as
containers that are rebuilt against llama.cpp master. Chasing that in nixpkgs is the wrong fight.

**The line is not "nixpkgs versus containers", it is per layer**, and the NPU is the case that proves
it. Its enablement (XRT, the XDNA shim, udev rules, memlock limits, kernel module) is host plumbing
that belongs in NixOS and would be miserable to reproduce by hand. Community flakes already do it,
and one of them packages Lemonade and FastFlowLM too. See
[references](#community-and-nixos-references). So: **runtime in a container, enablement in Nix.**

This repo already has the pattern: `overlays/containers.nix` pins image tags and
`virtualisation.oci-containers` declares the units, as five services already do. Note it runs
**podman**, not docker; the images are identical and upstream `docker run` flags translate directly.
Set `virtualisation.oci-containers.backend = "docker"` on this host only if some upstream tool
genuinely assumes the docker socket.

**Two tiers, deliberately.** Tier 1 is the declared container that hermes depends on, pinned and
committed. Tier 2 is a scratch directory where `docker compose up` runs whatever is being evaluated
this week, unmanaged by Nix and free to be messy. Promote from 2 to 1 only when something survives
contact. The point is that experiments cannot break the assistant, which is what makes it safe to
experiment quickly.

## Runtime landscape

The space is layered, and conflating the layers is what makes it confusing:

**Engines** do the math: `llama.cpp` (GGUF; Vulkan, ROCm, CPU), `vLLM` (ROCm, real batching),
`FastFlowLM` (XDNA 2 NPU only), plus `whisper.cpp`, `stable-diffusion.cpp`, `Kokoro` for the
non-LLM modalities.

**Servers** expose an API and manage models and processes on top of an engine:

|                                | What it is                                                                                                                                   | Fit here                                                                                                                                  |
| ------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| **Lemonade**                   | AMD-sponsored orchestrator over llama.cpp, FastFlowLM, whisper.cpp, sd.cpp, Kokoro, vLLM. OpenAI + Ollama + Anthropic compatible. Apache 2.0 | Strix Halo is its flagship target. Only Linux path to the NPU. Covers 3 of the 4 scoped workloads in one service                          |
| **Ollama**                     | Own llama.cpp fork plus a model registry. What the fleet runs today                                                                          | Familiar, but its ROCm story on `gfx1151` is a long-tail case rather than a first-class one, and it adds nothing here that Lemonade lacks |
| **llama.cpp** (`llama-server`) | The engine's own server, `--jinja` plus per-model tool-call parsers                                                                          | The reference for tool-call debugging. Fewest layers                                                                                      |
| **llama-swap**                 | Proxy that starts/stops `llama-server` instances on demand. In nixpkgs                                                                       | Pairs with llama.cpp when several models must coexist                                                                                     |
| **vLLM**                       | Throughput server, real batching                                                                                                             | Built for concurrency this box will not see. In nixpkgs                                                                                   |
| **ramalama**                   | Container-per-model, OCI-native                                                                                                              | Same instinct as tier 2 above, less mature ecosystem                                                                                      |

**Lemonade is not a competitor to llama.cpp; it sits above it.** It competes with Ollama. Choosing
Lemonade does not give up llama.cpp, it wraps it.

### On "llama.cpp is much faster than Ollama"

Widely repeated, and the 30-70% figures behind it are historical. Current measurements put llama.cpp
3-10% ahead on NVIDIA and 2-7% ahead on Apple Silicon with identical Q4_K_M files, with Ollama's Go
wrapper costing 5-15%. The gap is small because **Ollama runs the same llama.cpp core underneath**;
the difference is server overhead, not a different engine.

The relevant part is not the throughput. In May 2026 Ollama shipped v0.30.0-rc15 reverting to
llama.cpp directly, because its in-house Go engine had fallen behind on newer architectures where
things were slower or broken. One of the named casualties was **structured output**, which is the
machinery tool calling depends on.

**This argument does not carry over to Lemonade**, which is the reason to be relaxed about the extra
layer. Ollama reimplemented serving around llama.cpp; Lemonade's `lemond` is a pure HTTP router that
spawns llama.cpp as a *separate subprocess*, so inference runs in an unmodified llama.cpp. The cost
on the request path is a localhost hop, sub-millisecond against ~18ms per token at 55 t/s. It also
fixes the structural problem rather than inheriting it: backends are pinned in
`backend_versions.json`, so llama.cpp moves when you decide, not when someone re-vendors it.

### Recommendation: `llama-server` first, Lemonade when you want its extras

Both are right answers at different times, and the ordering follows from the use case rather than
from performance.

**Phase 1: bare `llama-server`.** The top risk is tool-call reliability, and while learning what
"good" looks like you want zero layers between hermes and the chat template. `--jinja` and the
per-model tool-call parsers are the best-documented place on the internet to debug a malformed
`tool_call`. Add `llama-swap` only when more than one model has to be resident.

**Then Lemonade**, once a working baseline exists to compare against, at which point adopting it is a
measurable A/B rather than an act of faith. What it buys:

1. Vendor-tuned for this chip, so ROCm and Vulkan backend selection stops being your problem. The
   `HSA_OVERRIDE_GFX_VERSION` some community images set is a property of *that image's* ROCm build,
   not of the hardware.
2. Backends as separate processes, so a GPU driver hang does not take the API down.
3. LLM, STT, TTS and image generation behind one API, which is most of the "later" list.
4. The NPU path, without changing anything else.

**The switch is cheap in both directions**, which is what makes this ordering safe rather than a
commitment: both consume the same GGUF files and both speak OpenAI `/v1`, so moving is a `base_url`
change in hermes plus a different container tag.

Do not run Ollama here. Its advantages are ecosystem and familiarity; its weak spots are AMD APUs,
the NPU and structured output, which is precisely where this workload lives.

### Known-good configuration

This hardware has enough sharp edges that the community has converged on a documented stack. Take it
rather than rediscovering it; [references](#community-and-nixos-references) below.

**The one that will waste a day:** `-dio` is **required** for models larger than roughly 6GB. Without
it, loading does not run slowly, it **hangs outright**. Every model worth running on this box is over
that threshold.

Runtime flags for `llama-server`:

| Flag              | Why                                                                |
| ----------------- | ------------------------------------------------------------------ |
| `-dio`            | Required above ~6GB or the load hangs                              |
| `-ngl 999`        | Full offload to the iGPU                                           |
| `-fa on`          | Flash attention (rocWMMA on the HIP path)                          |
| `-b 2048 -ub 512` | Community-tuned batch sizes for this chip                          |
| `--cache-prompt`  | Reuses the prefix across agent turns, which is most of an MCP loop |
| `-np 1`           | One user; avoid raising it alongside speculative decoding          |
| `--jinja`         | Tool-call parsing, the whole point here                            |

If ever building rather than pulling a container, `-DGGML_HIP_NO_VMM=ON` is **essential**: HIP's
virtual memory management does not work on this GPU. Also `-DGPU_TARGETS=gfx1151` and
`-DGGML_HIP_ROCWMMA_FATTN=ON`.

On ROCm versus Vulkan, do not re-derive what is already published: a public benchmark grid covers
both across quants, model sizes and context windows on this exact chip. Note the picture is genuinely
mixed and has moved: Vulkan is reported to significantly outperform HIP for decode, while ROCm with
flash attention holds up better at long context. Since this workload is long-context and
TTFT-sensitive, read the grid at the sizes actually being run rather than trusting either headline.

`HSA_OVERRIDE_GFX_VERSION=11.5.1` is worth knowing about: `gfx1151` sits at Preview status in AMD's
compatibility list, and 11.5.1 *is* gfx1151, so this makes detection explicit rather than faking a
different architecture. Distinct from the `11.0.3` some community images set, which really is a
fallback to another target.

## The fleet contract

Today `fleet.ai.model` is documented as an "Ollama tag" pulled by "the Ollama host". **That
description is the coupling.** The contract that consumers actually need is:

- an OpenAI-compatible `base_url`
- an opaque model id string that the endpoint understands

Nothing in hermes, NextChat or the option's shape depends on Ollama specifically; only the prose
does. Restating the description in runtime-neutral terms makes swapping Ollama for Lemonade for
llama-swap a `base_url` change, which is precisely the freedom this host is being built to have.

For `fleet.ai.host`, see [the note below](#on-making-the-endpoint-a-fleet-fact).

## Phase 1: the assistant

Deliberately one thing:

1. NixOS host: kernel, GPU, firewall, static IP, node-exporter, users.
2. One `llama-server` container, one model, serving an OpenAI-compatible endpoint.
3. hermes' `base_url` repointed from `laptop` to `ai`.
4. `agent-vm`'s single LAN egress hole repointed.
5. Laptop Ollama retired.

Done when the assistant answers with working MCP tools and the laptop can be closed.

Model selection is part of phase 1, and the criterion is tool-call reliability rather than size or
speed. `qwen3.5:4b` exists only because it was the largest thing that fit 8GB at 64K context; the
Qwen 3 family is repeatedly the strongest local tool-caller, so the sensible move is a larger member
of the same family rather than a jump to something unfamiliar. Re-tune once measured.

### Measuring what matters

Throughput benchmarks are the easy thing to measure and the wrong thing to optimise. Measure instead:

- **Tool-call success rate** over a fixed set of representative assistant tasks, run enough times to
  see the tail. This is the number that decides the runtime and the model.
- **TTFT at realistic prompt sizes**, meaning a full system prompt plus tool schemas, not a bare
  "hello". This is what the NPU path would improve.
- Generation rate last, only to confirm it is not the constraint.

If a raw throughput comparison is ever needed (for example choosing between ROCm and Vulkan on a bare
llama.cpp container), the servers return the numbers directly; sample at short, ~8K and ~32K prompts,
because a single short-prompt number hides the crossover between backends.

## Later

Listed so they are not forgotten, explicitly not designed yet. Do not start any of these until phase
1 has been running unattended for a while.

- **Lemonade, then NPU prefill.** The TTFT win described above, and the reason to adopt Lemonade once
  phase 1 gives a baseline to A/B against. First experiment after phase 1.
- **Immich remote ML.** Configured in Immich's admin UI as a URL list tried in order, so keeping
  compute's local URL as the second entry degrades to slow rather than broken. Note this is *not*
  covered by Lemonade: Immich needs its own ML container speaking its own protocol, and upstream
  warns that version skew between the two hosts causes instability. nixpkgs cannot do ROCm here
  (`immich-machine-learning` reaches onnxruntime via `insightface`/`rapidocr`, and nixpkgs'
  `onnxruntime` exposes `cudaSupport` only), so ROCm means the upstream container.
- **Speech for Home Assistant.** Lemonade already serves whisper.cpp and Kokoro, but HA speaks the
  Wyoming protocol while Lemonade speaks OpenAI `/v1/audio/*`. Bridging that gap is the open
  question, not the inference.
- **Image generation.** Lemonade bundles `stable-diffusion.cpp`. Competes with a resident LLM for
  memory, so it wants the LLM side settled first.

## Host configuration

The parts NixOS owns, all of which need confirming on the actual hardware.

**Kernel**: a hard floor, not a preference. Kernels older than 6.18.4 carry a `gfx1151` stability
bug, and the known-good stack asks for in-tree `amdgpu` at 6.19.2 or newer. The fleet's 7.1 clears
both comfortably, so this is only a constraint on ever pinning backwards. Worth noting the contrast
with the [Beelink](../storage/beelink.md), which pins 6.18 LTS for ZFS: **do not copy that pin here.**
Nothing on this host pins the kernel, and it must stay that way.

**BIOS**: UMA framebuffer to its minimum (512MB), and **IOMMU enabled** (required for the NPU).

**GTT**: the iGPU reaches system RAM through a fixed BIOS carve-out plus dynamic GTT. For an
inference box the carve-out is wasted reservation, so minimise it and raise the GTT limit instead.
Recent kernels default GTT to roughly half of RAM; the old `amdgpu.gttsize` knob is deprecated in
favour of TTM's page limit, expressed in 4KiB pages, so `pages = GiB * 262144` and 120 GiB is
`31457280`. Which name the shipped kernel honours (`ttm.pages_limit` vs `amdttm.pages_limit`) has to
be checked on the box.

Verify with `rocm-smi --showmeminfo vram` that a large model actually lands on the GPU instead of
silently falling back to CPU.

Leave headroom. Handing the GPU 120 of 128GB leaves the OS 8GB, and a host under memory pressure
takes SSH down with it.

**Containers need device access**: `/dev/kfd`, `/dev/dri/card0` and `/dev/dri/renderD128` for the
GPU, with the container's user in the `video` and `render` groups and the render group GID matching
the host; `/dev/accel/accel0` additionally for the NPU. Model weights belong on a named volume on the
x4 NVMe so a container replacement does not re-download tens of GB.

## Network and exposure

The endpoint has no authentication, so reachability is the access control:

- Static IP, `ai` in `fleet.lan.hosts`
- The serving port open to `compute` and `laptop` only, scoped by IP so it survives interface renames
- `agent-vm`'s single LAN egress hole repointed from `laptop` to `ai`
- Nothing else on the LAN reaches it, and it is never published

`laptop` gets access for dev tooling hitting the endpoint directly rather than round-tripping through
hermes.

The port is the runtime's, not necessarily 11434. Lemonade's differs and community images disagree on
it, so confirm it at install time and let `agent-vm`'s egress rule follow.

## Monitoring

`prometheus-node-exporter` on :9100, scraped by compute, same shape as the microVM guests: a static
config with an `instance` label. Alerts inherit compute's existing `node` rules.

Neither node-exporter nor the fleet has an amdgpu collector, so GPU utilisation, VRAM and junction
temperature need a `rocm-smi` textfile collector or a dedicated exporter. Open item.

Thermals deserve a rule here: 130W sustained in a mini-PC chassis is a different regime from
compute's N150, and sustained inference is exactly the workload that finds a bad fan curve.

## Power

Always-on. `profiles/nixos/headless.nix` already forbids suspend and hibernate, and wake-on-demand
would put a cold start in front of every assistant reply.

Measure idle draw before accepting that permanently. If it lands high, revisit, because this box is
idle most of the day and the assistant tolerates latency better than the household tolerates a space
heater.

If it shares the UPS it joins the NUT client set, but the NUT server itself is moving to the
[Beelink](../storage/beelink.md), so sequence this after that migration rather than pointing at the
Synology.

## Fleet impact

New:

- `hosts/ai/` plus a `mkNixosHost` entry in `flake.nix`
- `hosts/shared.nix`: `lan.hosts.ai`
- `overlays/containers.nix`: the runtime image and its pinned tag
- `dotfiles-private`: host entry for sops bootstrap, `.sops.yaml` key
- Install via [`apps/nixos-install`](../../apps/nixos-install/README.md)

Repointed:

- `hosts/guests/agent-vm/services/hermes-agent.nix`: `base_url` off `laptop`
- `hosts/compute/microvm/guests.nix`: `egress.allowLan` host and port
- `modules/nixos/fleet/shared.nix`: `ai.model` description, currently Ollama-specific
- `hosts/compute/selfhost/monitoring/`: scrape target

Removed:

- `hosts/laptop/ollama.nix` and its import in `hosts/laptop/default.nix`
- `hosts/laptop/hardware/nvidia.nix`: `nvidiaPersistenced` exists to stop Ollama's CUDA dropping to
  CPU after idle suspend. Keep or drop it deliberately, do not leave the stale reason in place.

Docs:

- `hosts/compute/README.md`: the architecture diagram says `agent-vm → laptop Ollama (private)`
- `hosts/guests/agent-vm/README.md`: "the model runs on the laptop"

### On making the endpoint a fleet fact

The obvious cleanup is `fleet.ai.host`, so the repoint is one line in `hosts/shared.nix`. It only
reaches two of the three call sites.

`hosts/compute/microvm/guests.nix` is imported by `hosts/shared.nix` (to seed `microvms.compute`), so
it cannot read `custom.fleet` without a cycle. Its `egress.allowLan` entry stays a literal, or
`hosts/compute/microvm/host.nix` grows module arguments and overrides it there.

Either add `fleet.ai.host` and accept one literal, or leave all three literal and change them
together. Do not half-adopt it: an endpoint that is a fleet fact in one file and a string in another
is worse than either.

## Community and NixOS references

This hardware is popular enough with local-LLM people that most of the sharp edges are already
documented. Read these before building anything.

**Strix Halo specifically:**

| Reference                                                                              | Why it matters                                                                                                                                         |
| -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| [kyuz0/amd-strix-halo-toolboxes](https://github.com/kyuz0/amd-strix-halo-toolboxes)    | Containers rebuilt against llama.cpp master, ROCm 6.4/7.x and Vulkan (RADV, AMDVLK). The most direct candidate for the phase 1 container               |
| [Its benchmark grid](https://kyuz0.github.io/amd-strix-halo-toolboxes/)                | Backend comparison across quants, sizes and context windows **on this exact chip**. Replaces most of the benchmarking this doc would otherwise ask for |
| [llama.cpp discussion #20856](https://github.com/ggml-org/llama.cpp/discussions/20856) | The known-good ROCm + llama.cpp stack. Source of the `-dio` and `NO_VMM` gotchas above                                                                 |

**NixOS specifically.** `nixos-hardware` has **no** Strix Halo or Ryzen AI Max module (it carries
older Minisforum UM690/UM790 profiles, which do not apply), so there is no ready-made profile to
import. What exists instead are community flakes:

| Flake                                                                             | Provides                                                                                                                                       | Caveat                                                                                                                                            |
| --------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| [noamsto/nix-amd-ai](https://github.com/noamsto/nix-amd-ai)                       | NixOS module packaging XRT, the XDNA plugin, FastFlowLM **and Lemonade**, plus ROCm/Vulkan llama.cpp, whisper.cpp, sd.cpp. Actively maintained | Built and tested on Strix **Point** only; Halo is documented but **untested**, and its Halo memory values are explicitly "guidance, not measured" |
| [robcohen/nix-amd-npu](https://github.com/robcohen/nix-amd-npu)                   | XRT plus XDNA driver, udev rules, memlock limits                                                                                               | Narrower scope, NPU only                                                                                                                          |
| [skitzo2000/nix-xdna](https://github.com/skitzo2000/nix-xdna)                     | Out-of-tree XDNA module, firmware, userspace tools                                                                                             | Lists Strix Halo as untested. Largely redundant given `amdxdna` is in-tree at 7.1                                                                 |
| [hellas-ai/nix-strix-halo](https://github.com/hellas-ai/nix-strix-halo)           | ML libraries against `gfx1151` via TheRock ROCm SDK                                                                                            | Workspace flake rather than a host module                                                                                                         |
| [demyanrogozhin/nix-llama-rocm](https://github.com/demyanrogozhin/nix-llama-rocm) | NixOS service modules for llama.cpp, including RPC server                                                                                      | Smaller, worth reading rather than depending on                                                                                                   |

**`nix-amd-ai` is the one to watch**, because it covers exactly the layer that is painful to do by
hand (XRT plus XDNA shim plus memlock plus udev) and it already packages Lemonade. Treat it as a
reference implementation to read now and a candidate input when the NPU phase starts, not a
dependency to adopt sight-unseen on hardware its author has not tested.

Also worth tracking: [nixpkgs#472876](https://github.com/nixos/nixpkgs/issues/472876), a memory
access fault in `torchWithRocm` on `gfx1151`. Not on the phase 1 path, but Immich ML is a torch
workload, so it lands squarely on the "later" list.

## Decisions

Settled, recorded so they are not relitigated:

- **Runtime: `llama-server` in a container for phase 1**, most likely the Strix Halo toolbox image.
  Lemonade deferred to the NPU phase, when a working baseline exists to A/B against. **Ollama is not
  used on this host.** Rationale in [the runtime section](#runtime-landscape); the switch between
  them is a `base_url` and a container tag, so this is cheap to revisit.
- **Containers for the inference runtime, NixOS for host enablement.** Not a wholesale move to
  Docker.
- **Backends are not pinned by this document.** ROCm versus Vulkan is read off the published
  benchmark grid at the sizes actually being run, not re-derived here.

## Open items

1. Confirm the container image tag and its port at install time, then set the firewall and
   `agent-vm` egress rule to match.
2. Correct GTT knob for the shipped kernel, and confirm large models land on the GPU.
3. Model choice for phase 1, decided on tool-call reliability rather than size.
4. Disk layout across the x4 and x1 M.2 slots. Models on the x4.
5. amdgpu metrics exporter choice.
6. Whether `ai` joins the UPS client set, after the NUT server moves to the Beelink.
7. Hostname: `ai` throughout this document, not yet decided.
8. Idle power draw, which decides whether always-on survives contact with the electricity bill.
9. Whether `nix-amd-ai` becomes a flake input for the NPU phase or just a reference to crib from,
   given Halo is untested there.
