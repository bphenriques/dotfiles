# AI Host

NixOS box running local inference for the fleet: Ollama serves an OpenAI-compatible endpoint on the
iGPU and ComfyUI serves image generation. `agent-vm`'s hermes, the chat UI and the laptop's coding
agent all reach them through `fleet.ai`. Powered on over Wake-on-LAN when needed; suspend is off,
so it is either running or off. What depends on it fails per request meanwhile and recovers on its own.

## Hardware

- **Model**: Minisforum MS-S1 MAX
- **CPU**: AMD Ryzen AI Max+ 395 (Strix Halo), 16 cores / 32 threads
- **GPU**: Radeon 8060S iGPU, plus an XDNA 2 NPU that nothing uses yet
- **RAM**: 128GB LPDDR5X-8000, soldered
- **Storage**: 2TB NVMe holding both the OS and the models (the second slot is x1 and empty)
- **Network**: dual 10GbE, one cabled. ~130W sustained
- **UPS**: EATON Ellipse ECO 650, monitored over the network from [storage](../storage/README.md)

## Architecture

```
  compute  ──┐                                     ┌──▶ Ollama  :11434  (chat, coding, hermes)
  laptop   ──┼──▶ nftables, per-host allow ────────┤
  agent-vm ──┘    nothing behind it authenticates  └──▶ ComfyUI :8000   (image generation)
   (via compute's NAT)                                        │
                                                              ▼
                                          Radeon 8060S iGPU, one pool, no arbitration

  node + smartctl exporters ────────────────────────▶ compute Prometheus
  upsmon ───────────────────────────────────────────▶ storage NUT
```

Neither endpoint authenticates, so reachability is the access control and `firewall.nix` is the whole
gate: compute and laptop, nothing else. `agent-vm` egresses through compute's NAT, which is why
compute is on the list and why every guest on compute reaches the endpoints too.

Ollama and ComfyUI share the one iGPU with no arbitration between them, so a long image generation
stalls chat and coding for its duration.

## Models

`fleet.ai` declares the models the fleet depends on and they are pulled on boot. Anything pulled by
hand survives deploys and reboots until removed; `ollama-models {list,pull,rm}` on compute is the way
to manage those.

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

## Post-Install

### ComfyUI

Model weights are a manual download, tens of GB behind interactive menus, and nothing generates until
they are there. Entries 1 to 4 pull the two models the chat UI uses, each with its faster variant:

```bash
podman exec -it comfyui /opt/get_qwen_image.sh <n>
```

## References

Strix Halo is niche enough that the sharp edges live in a handful of places:

- [kyuz0/amd-strix-halo-toolboxes](https://github.com/kyuz0/amd-strix-halo-toolboxes) and its
  [benchmark grid](https://kyuz0.github.io/amd-strix-halo-toolboxes/): backend comparison on this
  exact chip, where ROCm against Vulkan is genuinely mixed
- [llama.cpp discussion #20856](https://github.com/ggml-org/llama.cpp/discussions/20856): the
  known-good ROCm stack
- [noamsto/nix-amd-ai](https://github.com/noamsto/nix-amd-ai): XRT, the XDNA plugin and Lemonade, the
  only Linux path to the NPU
