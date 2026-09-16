# AI Host

Minisforum MS-S1 MAX running local inference for the fleet. Ollama serves an OpenAI-compatible
endpoint on the iGPU; `agent-vm`'s hermes, the chat UI and the laptop's coding agent all point at it
through `fleet.ai.endpoint`.

**The success metric is tool-call reliability, not tokens/second.** An assistant that generates fast
but emits a malformed `tool_call` one time in five is useless, and time-to-first-token is the latency
an agent loop actually exposes. Pick models and runtimes on that basis; treat throughput as a
constraint to satisfy rather than a number to maximise.

## Hardware

- AMD Ryzen AI Max+ 395 (Strix Halo), 16 Zen 5 cores / 32 threads, `gfx1151` Radeon 8060S iGPU
- XDNA 2 NPU, working out of the box (`amdxdna` bound, `/dev/accel/accel0`). Unused so far
- 128GB LPDDR5X-8000 soldered, 123 GiB visible to the OS
- One 2TB Kingston NVMe in the PCIe 4.0 x4 slot. **The x1 slot is empty**, so OS and models share it
- Dual RTL8127 10GbE, `enp97s0` cabled. Internal 320W PSU, ~130W sustained

**Memory bandwidth is the ceiling, not compute.** Token generation reads the whole model once per
token, so `t/s ≈ bandwidth / model bytes`: at ~256 GB/s a dense 70B at q4 tops out near 6 t/s no
matter what runs it, while an MoE only touches its active experts. **Prefer MoE.** The 128GB buys
model *size*, not model *speed*.

## Measured, do not re-derive

- **GTT is the memory that matters, not the BIOS carve-out.** The iGPU reaches system RAM through a
  fixed carve-out (2048 MiB here) plus dynamic GTT. The knob is `ttm.pages_limit`, in 4KiB pages, so
  `pages = GiB * 262144`. It defaulted to 61.7 GiB, under gpt-oss-120b at MXFP4, and is set to
  112 GiB (`29360128`). **Raising the BIOS carve-out moves memory out of the flexible pool rather
  than adding any**, and Ollama reports `total="112.0 GiB"`, so it counts GTT. Leave BIOS alone.
- `rocm-smi --showmeminfo vram` reports only the carve-out, so it shows ~2 GiB no matter how large a
  model is resident. It is the wrong tool for "did the model fit"; the runtime's own log line is right.
- **`gfx1151` is detected natively** by `ollama/ollama:*-rocm` (ROCm 7.2). `HSA_OVERRIDE_GFX_VERSION`
  is not needed; advice to force `11.0.3` is about older images.
- IOMMU is already on from BIOS, so no `amd_iommu` kernel param.
- The GPU card node is **`card1`**, and `/dev/kfd` and `renderD128` are mode 0666, so containers need
  no group juggling.
- **Pin the kernel forwards.** Unpinned lands on nixpkgs' 6.18 LTS default, below the 6.19.2 the
  amdgpu stack wants; the first install proved it by booting 6.18.45. `compute` and `laptop` pin
  `linuxPackages_7_1` explicitly, so "the fleet runs 7.1" is not a property you inherit. Never copy
  storage's 6.18 ZFS pin here.
- The `.link` file matches by MAC, which applies before udev picks the predictable name, so the
  interface comes up as `eth0`. Same as storage's `eth1`. Everything matches by MAC, so nothing
  depends on the name.

## Decisions

Settled, recorded so they are not relitigated:

- **Ollama in a container, not from nixpkgs.** Containers own the inference server and its ROCm
  userspace; NixOS owns the kernel, firmware, GPU enablement, firewall and the pinned image tag.
  Chasing a weekly-moving stack through nixpkgs is the wrong fight.
- **Ollama rather than bare `llama-server`**, chosen for continuity: hermes' `provider = "ollama"`,
  the chat UI and the `fleet.ai.model` tags already speak it, so containerising the same server changed
  only the address. Watch, do not re-argue: Ollama's structured output is the machinery tool calling
  runs on and is its documented weak spot, and `gfx1151` is long-tail on its ROCm path.
  `llama-server` is the A/B if tool calls disappoint. Lemonade is the later option, and the only
  Linux path to the NPU.
- **ComfyUI comes from a container too, and for a stronger reason than Ollama's.** nixpkgs has
  `comfyui` and a `services.comfyui` module, but the package pins torch to `cudaPackages_13`; built
  with `config.rocmSupport` it wants `torch`, `torchvision`, `torchaudio` and `rocm-merged` compiled
  from source, on top of 6.0 GiB of fetches, and again on every nixpkgs bump. The image used instead
  installs torch from AMD's ROCm nightlies as `torch[device-gfx1151]`, built for this chip alone.
- **The switch is cheap in both directions.** Every candidate eats the same GGUF weights and speaks
  OpenAI `/v1`, so moving is a `fleet.ai.endpoint` change plus a container tag.

## Network and exposure

The endpoint has no authentication, so reachability is the access control. `firewall.nix` opens
11434 to `compute` and `laptop`, and the exporters to `compute` alone. `agent-vm` reaches it through
compute's NAT egress, which is why compute is on the list.

**Ollama runs with `--network=host`, not a published port.** Netavark DNATs published ports in
`nat prerouting`, before the input hook, and `nixos-fw` has no forward chain, so an `input-allow`
rule for a published port is dead code. Measured: storage reached 11434 while blocked on 9100.

**Anything that reaches 11434 can `POST /api/pull`**, so every container and guest on compute can
pull arbitrary models onto the disk. Narrowing below host granularity is authentication, not
firewalling: compute SNATs guest egress to bond0. `hosts/compute/microvm/guests.nix` is the lever.

**ComfyUI on 8000 has no authentication either, and the same guests reach it.** Its API is the wider
surface of the two: it takes file uploads into the input directory and executes whatever graph it is
handed. Open to compute for the chat UI and to laptop for ComfyUI's own UI, on the same reasoning
that reachability is the access control.

**Hardening flags belong in `extraOptions`, not `containers.conf`**: a deploy rewrites the file but
does not recreate a running container. `no_new_privileges` is not a `containers.conf` key at all and
podman drops it silently; `--security-opt=no-new-privileges` is the working form.

## Models

`fleet.ai` declares the models the fleet depends on and `ollama-configure` pulls them. The unit
prunes only what it pulled itself, tracked in `/var/lib/ollama-configure/managed`, and never lists
`/api/tags`, so a model pulled by hand survives deploys and reboots until you remove it.

`ollama-models` on compute wraps the API for that: `list`, `pull <model>`, `rm <model>`.

Coding agents need no configuration to see the result: omp discovers every tag and offers it as
`ollama/<tag>`, after an `omp models refresh` since it caches the list. `OLLAMA_MAX_LOADED_MODELS` is
the count of declared models, so an extra resident model evicts one of them and costs it a cold load.

## Image generation

ComfyUI from [`kyuz0/amd-strix-halo-comfyui`](https://github.com/kyuz0/amd-strix-halo-comfyui-toolboxes),
serving its own UI on 8000 and, once configured, the chat UI's generate and edit buttons.

**The pin is untracked.** Upstream cuts no GitHub releases, only datestamped tags, so
`overlays/containers.nix` gives it no `updateInfo` and `check-updates` cannot see it. Bumping is a
manual read of the tag list.

**It is a toolbox image**: `Cmd` is a bare `/bin/bash` and the ROCm environment comes from
`/etc/profile.d`, which only a login shell reads, so `services/comfyui.nix` supplies the launch line
and those variables itself. `--disable-mmap` is not optional: mmap above 64GB is pathologically slow
on gfx1151.

**Weights are a manual bootstrap**, tens of GB behind interactive menus. These four pull exactly what
the two workflows in `open-webui/` name, and nothing works until they are present:

```
podman exec -it comfyui /opt/get_qwen_image.sh 2   # Qwen-Image-Edit 2511 fp8, + text encoder and VAE
podman exec -it comfyui /opt/get_qwen_image.sh 4   # its Lightning 4-step LoRA
podman exec -it comfyui /opt/get_qwen_image.sh 1   # Qwen-Image 2512 fp8, for text-to-image
podman exec -it comfyui /opt/get_qwen_image.sh 3   # its Lightning 4-step LoRA
```

Entries 1 and 2 share the text encoder and VAE, so the second pair adds only two files. The volume is
`comfyui` at `/opt/comfy-home`, holding `comfy-models`, `comfy-outputs` and the HuggingFace cache;
`/opt/model_manager.py` lists what is there. It is not at `/root` because Fedora ships that mode 0550,
writable only via `CAP_DAC_OVERRIDE`, which the container's `--cap-drop=ALL` removes.

**Take the API-format workflows, not the UI ones.** Only `workflows/API/*.json` are the format
Open-WebUI posts, and upstream ships those for the safetensors path only, which is why the pick is
fp8 rather than the GGUF the other menu entries offer. Both copies here swap the unet to fp8, and the
text-to-image one also swaps in the LoRA matching its own model: upstream's JSON names the Edit LoRA
in a text-to-image graph, which reads as a copy-paste slip.

**Video is not reachable from the chat UI.** Open-WebUI has no video generation of any kind, so there
is nothing to wire. The container already carries the workflows and `/opt/get_wan22.sh`,
`get_ltx2.sh`, `get_hunyuan15.sh` and `get_minimax_h3.sh`, so it is a download away in ComfyUI's own
UI on 8000 and needs no change here.

**ComfyUI and Ollama share one GTT pool and one iGPU with no arbitration.** `OLLAMA_KEEP_ALIVE=-1`
pins about 40 GB against a 112 GiB limit, and a diffusion run stalls chat and coding for its duration.
If that grates, the lever is Ollama's keep-alive, not a bigger carve-out.

## Monitoring

`prometheus-node-exporter` on :9100 and `smartctl` on :9633, scraped by compute from
[`monitoring/ai.nix`](../compute/selfhost/monitoring/ai.nix), with an `ai (inference)` row in the
merged Grafana dashboard.

**hwmon already covers the GPU**: the amdgpu chip publishes `PPT` as `node_hwmon_power_watt` and
`sclk` as `node_hwmon_freq_freq_mhz` alongside `edge` temperature, so package power and clock need no
extra exporter. VRAM occupancy and utilisation are still missing and would need a `rocm-smi` textfile
collector.

**Only `hwmon` and `systemd` are enabled.** `rapl` needs root to read `energy_uj` and `thermal_zone`
returns zero series on this board, so both were dropped after measuring rather than left on as
plausible-looking dead weight. compute grants `CAP_DAC_READ_SEARCH` for rapl; not worth a capability
here when hwmon's PPT reports the same number.

The OS and every model share one consumer NVMe, so `smartd` and the smartctl exporter run here too.
The seven `disk-health` rules in `monitoring/smartctl.nix` are host-agnostic and cover it as soon as
the exporter exists; hwmon `Composite` temperature alone was not wear or reallocation coverage.

Alert thresholds are per sensor, not the fleet-wide `max(node_hwmon_temp_celsius) > 80`, which is now
scoped to `instance="compute"` rather than merely claimed to be superseded here. That rule
takes `max()` across every sensor, conflating parts with very different limits: NVMe crit is 89.85C,
CPU Tjmax is 100C. CPU warns at 95C, GPU at 90C, NVMe at 75C, all `for: 10m`.

Measured for calibration on gpt-oss:20b, since dropped: peaks at **CPU 60C, GPU 48C, 104W
PPT, sclk pinned at 2900 MHz** with no throttling, against 5W and 600 MHz at idle. So the cooling has
a lot of headroom and these thresholds are far from normal operation, which is the point. hwmon names chips by PCI path, so the rules join `node_hwmon_sensor_label` on
`(chip, sensor)` to match `Tctl` / `edge` / `Composite` by name. No alert on the endpoint being down:
a dead assistant is noticed immediately.

## Power

Always-on; `profiles/nixos/headless.nix` forbids suspend, and wake-on-demand would put a cold model
load in front of every reply. Wake-on-LAN is on so the box can be recovered after a shutdown.

**Idle is 5.05W package** with the GPU at 600 MHz. That is the SoC only, so it is a floor rather than
wall draw. The governor is already `amd-pstate-epp` / `powersave` / `balance_performance`, which is
the target state and not a default to fix. ASPM is left at the BIOS default: forcing `powersave`
might save a watt against a real history of marginal PCIe devices dropping out. WiFi, Bluetooth and
the HDA controller are blacklisted, being useless on a headless wired box.

**No zram or swap**, unlike storage. `systemd-oomd` is on by default, the box sits near 2% memory
use, and the plausible failure here is a GPU allocation rather than OS memory pressure. Compressing
anonymous pages to buy RAM is beside the point when 112 GiB is reserved for GTT.

NUT client of the [Beelink](../storage/README.md)'s Ellipse ECO 650. Under sustained inference this
box roughly doubles the UPS load, which is fine for NUT's job of shutting down cleanly rather than
riding through.

## Later

Not designed, listed so they are not forgotten. Do not start any until phase 1 has run unattended
for a while.

- **Lemonade.** The only Linux path to the NPU, but not a speed play: `docs/measurements.md` measures
  NPU prefill at roughly a quarter of this iGPU's. Worth it only if something needs the NPU itself.
- **Immich remote ML.** Not covered by Lemonade: Immich needs its own container and protocol, and
  nixpkgs cannot do ROCm there (`onnxruntime` exposes `cudaSupport` only).
- **Speech for Home Assistant.** HA speaks Wyoming, Lemonade speaks OpenAI `/v1/audio/*`. Bridging
  that is the open question, not the inference.

## References

Strix Halo is popular enough with local-LLM people that the sharp edges are documented.

| Reference                                                                                                                                                       | Why                                                                                                                                                                  |
| --------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [kyuz0/amd-strix-halo-toolboxes](https://github.com/kyuz0/amd-strix-halo-toolboxes) and its [benchmark grid](https://kyuz0.github.io/amd-strix-halo-toolboxes/) | Backend comparison across quants, sizes and context windows **on this exact chip**. ROCm vs Vulkan is genuinely mixed; read the grid at the sizes actually being run |
| [llama.cpp discussion #20856](https://github.com/ggml-org/llama.cpp/discussions/20856)                                                                          | The known-good ROCm stack. Source of `-dio` being **required** above ~6GB or loading hangs outright                                                                  |
| [noamsto/nix-amd-ai](https://github.com/noamsto/nix-amd-ai)                                                                                                     | Packages XRT, the XDNA plugin, FastFlowLM and Lemonade. Read it when the NPU phase starts; Halo is untested there                                                    |

`nixos-hardware` has no Strix Halo module, so there is no ready-made profile to import.
