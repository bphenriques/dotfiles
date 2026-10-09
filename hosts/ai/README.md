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
