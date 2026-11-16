# AI Host

NixOS box running local inference for the fleet: llama-swap serves one OpenAI-compatible endpoint for chat,
coding, images and embeddings, each model a [Gufo](https://github.com/gufo-org/gufo) container on the iGPU,
and whisper.cpp (native, Vulkan) serves speech-to-text.
The chat UI and the laptop's coding agent reach them through `fleet.ai`. It also hosts the sealed
[`agent-alfredo-vm`](microvm/guests/agent-alfredo-vm), which reaches them through its bridge gateway. Powered on over
Wake-on-LAN when needed; suspend is off, so it is either running or off.
What depends on it fails per request meanwhile and recovers on its own.

## Hardware

- **Model**: Minisforum MS-S1 MAX
- **CPU**: AMD Ryzen AI Max+ 395 (Strix Halo), 16 cores / 32 threads
- **GPU**: Radeon 8060S iGPU, plus an XDNA 2 NPU that nothing uses yet
- **RAM**: 128GB LPDDR5X-8000, soldered
- **Storage**: 2TB NVMe holding both the OS and the models (the second slot is x1 and empty)
- **Network**: dual 10GbE, one cabled. ~130W sustained
- **UPS**: EATON Ellipse ECO 650, monitored over the network from [storage](../storage/README.md)

## Fan curve

The board exposes no hwmon PWM, no tachometer and no ACPI fan object, so the EC's own curve tables are
the only control surface. Stock sits at a flat 20% from 25C up with no fan-stop point, which is the
whole of the idle noise: at a 36C idle there is no lower band to drop into. TDP modes, governor and
C-states are all dead ends for it.

[`fan-curve.nix`](hardware/fan-curve.nix) rewrites only the two records below 45C. Everything from 55C
up is left stock, so behaviour under load is unchanged. Raising that end is parked in
[`TODO.md`](TODO.md).

| Temp  | 25C | 45C | 55C | 65C | 75C | 85C | 90C |
| ----- | --- | --- | --- | --- | --- | --- | --- |
| Stock | 20% | 20% | 22% | 23% | 25% | 28% | 32% |
| Now   | 10% | 10% | 22% | 23% | 25% | 28% | 32% |

The EC reverts to stock on power cycle, hence the boot-time oneshot. Firmware re-reads the tables
continuously, so `fan-idle-duty <percent>` applies immediately and is how to find the stall floor by
ear, there being no tachometer to read. Values set that way are lost on reboot; the declared one is
`runtimeEnv.IDLE_DUTY`. Going too low can only stall the fans below 45C, where the untouched 22%
record takes over. 10% is quiet and holds temperature at 38.5C idle, 2C above stock. Community profiles
for this board put idle between 6% (`ultrasilenzioso`) and 12% (`silenzioso`), so 10% is ordinary and
going down to 6% is known not to stall these fans.

A BIOS update is not a fan fix: 1.10 and 1.11 are EC firmware bumps plus a Windows WOL fix. An EC
firmware change could move these offsets, so the script vets each record's temperature before writing.

## Architecture

```
  compute         ──┐                                     ┌──▶ llama-swap :11434  (chat, coding, images, embeddings)
  laptop          ──┼──▶ nftables, per-host allow ────────┤         │ starts and stops one Gufo container per model
  agent-alfredo-vm ─┘    nothing behind it authenticates  └──▶ whisper    :8090   (speech-to-text)
   (bridge, inference + whisper only)                               │
                                                                    ▼
                                                     Radeon 8060S iGPU, one pool

  node + smartctl exporters ────────────────────────▶ compute Prometheus
  upsmon ───────────────────────────────────────────▶ storage NUT
```

No endpoint authenticates, so reachability is the access control and `firewall.nix` is the whole
gate: compute and laptop, nothing else. The guest's seal opens it only inference and whisper, on the bridge
address; see [`microvm/`](microvm) for its holes.

llama-swap decides which models share the iGPU (see [Memory](#memory)); whisper's model sits in the
separate VRAM carve-out.

## Models

`fleet.ai` names the models and [`llama-swap.nix`](services/llama-swap.nix) serves them under those ids.
[`models.nix`](services/models.nix) pins every file by revision and sha256; `llama-swap-configure` downloads
what is missing on boot and trusts what is present. Gufo has kernels per model, so a quant or fine-tune it
does not list may not load: Flash-Next only runs unsloth's UD-Q4_K_XL.

`qwen3.8-27b-uncensored` is gated behind a click-through on Hugging Face, so its two files are placed by hand
in `/var/lib/llama-swap/models/qwen3.8-27b-uncensored/` (names and hashes in `models.nix`).

| Model                    | Serves                                       | GTT    |
| ------------------------ | -------------------------------------------- | ------ |
| `qwen3.8-flash-next`     | Alfredo, the chat UI, coding (two sessions)  | 90 GiB |
| `qwen3.8-27b-uncensored` | the chat UI, with vision                     | 24 GiB |
| `qwen-image-2.1-turbo`   | image generation and editing                 | 32 GiB |
| `embeddinggemma-2`       | the chat UI's document search                | 1 GiB  |

## Memory

The iGPU's pool (`ttm.pages_limit`) is carved from the same 123.5 GiB of RAM, so overcommitting it means
the kernel OOM killer, or a panic when killed GPU processes do not release memory (2026-11-05, 2026-11-06).
llama-swap's matrix allows two combinations and swaps between them, so a request can only be slow, never
overcommit:

| Set      | Models                                 | GTT      | RAM left |
| -------- | -------------------------------------- | -------- | -------- |
| `text`   | Flash-Next, embeddings                 | 91 GiB   | 24 GiB   |
| `images` | uncensored 27B, Qwen-Image, embeddings | 57 GiB   | 59 GiB   |

The cap is 96 GiB: `text` plus headroom. Flash-Next is preloaded at boot. A swap takes 25-40 s with the
weights in page cache, so the first chat after an image (or the first image after a chat) waits that long.

## Monitoring

Node and disk exporters scraped by compute, with an `ai (inference)` row in the merged Grafana
dashboard. Alert rules live in [`monitoring/ai.nix`](../compute/selfhost/monitoring/ai.nix), with
temperature thresholds per sensor rather than the fleet-wide one, since CPU, GPU and NVMe throttle at
very different points.

## Setup

| Dependency | What                                                   | Reference                                                  |
| ---------- | ------------------------------------------------------ | ---------------------------------------------------------- |
| NUT        | upsmon credentials against storage's UPS               | [storage](../storage/README.md)                            |
| Private    | LAN MAC                                                | `dotfiles-private/hosts/ai/settings.nix`                   |
| Secrets    | Bootstrap via `dotfiles-secrets init-host` (Bitwarden) | [`apps/nixos-install`](../../apps/nixos-install/README.md) |

Run [`apps/nixos-install`](../../apps/nixos-install/README.md): disko partitioning, secrets provisioning, NixOS install.

## References

Strix Halo is niche enough that the sharp edges live in a handful of places:

- [kyuz0/amd-strix-halo-toolboxes](https://github.com/kyuz0/amd-strix-halo-toolboxes) and its
  [benchmark grid](https://kyuz0.github.io/amd-strix-halo-toolboxes/): backend comparison on this
  exact chip, where ROCm against Vulkan is genuinely mixed
- [llama.cpp discussion #20856](https://github.com/ggml-org/llama.cpp/discussions/20856): the
  known-good ROCm stack
- [noamsto/nix-amd-ai](https://github.com/noamsto/nix-amd-ai): XRT, the XDNA plugin and Lemonade, the
  only Linux path to the NPU
