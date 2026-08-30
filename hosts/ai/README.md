# AI Host

Minisforum MS-S1 MAX running local inference for the fleet. Ollama serves an OpenAI-compatible
endpoint on the iGPU; `agent-vm`'s hermes, NextChat and the laptop's coding agent all point at it
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
  NextChat and the `fleet.ai.model` tags already speak it, so containerising the same server changed
  only the address. Watch, do not re-argue: Ollama's structured output is the machinery tool calling
  runs on and is its documented weak spot, and `gfx1151` is long-tail on its ROCm path.
  `llama-server` is the A/B if tool calls disappoint. Lemonade is the later option, and the only
  Linux path to the NPU.
- **The switch is cheap in both directions.** Every candidate eats the same GGUF weights and speaks
  OpenAI `/v1`, so moving is a `fleet.ai.endpoint` change plus a container tag.

## Network and exposure

The endpoint has no authentication, so reachability is the access control. `firewall.nix` opens
11434 to `compute` and `laptop` only, and 9100 to `compute` alone. `agent-vm` reaches it through
compute's NAT egress, which is why compute is on the list. Nothing else on the LAN reaches it and it
is never published.

**Know what that grants.** Anything that can reach 11434 can also `POST /api/pull`, which fetches
arbitrary models from the internet onto the 2TB disk, and can run inference at will. Since compute is
on the allowlist, that includes every container and guest on compute, not just hermes. Acceptable for
a household LAN and the reason the port is not open wider; it is the residual risk to weigh before
ever widening it.

The container runs unprivileged with **all capabilities dropped** and `no_new_privileges`, verified
by running the image with `--cap-drop=ALL` and confirming it still binds and drives the GPU.

## Monitoring

`prometheus-node-exporter` on :9100, scraped by compute from
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

Alert thresholds are per sensor, not the fleet-wide `max(node_hwmon_temp_celsius) > 80`. That rule
takes `max()` across every sensor, conflating parts with very different limits: NVMe crit is 89.85C,
CPU Tjmax is 100C. CPU warns at 95C, GPU at 90C, NVMe at 75C, all `for: 10m`.

Measured under load for calibration: generating on gpt-oss:20b peaks at **CPU 60C, GPU 48C, 104W
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

NUT client of the [Beelink](../storage/README.md)'s Ellipse ECO 650. Under sustained inference this
box roughly doubles the UPS load, which is fine for NUT's job of shutting down cleanly rather than
riding through.

## Later

Not designed, listed so they are not forgotten. Do not start any until phase 1 has run unattended
for a while.

- **Model choice.** `qwen3.5:4b` was sized for the laptop's 8GB and is kept only so the move here
  changed one variable. Pick against tool-call reliability, preferring a larger MoE.
- **Lemonade, then NPU prefill.** The NPU does prefill while the iGPU decodes, roughly halving TTFT,
  which is the metric that matters. Lemonade is the only Linux path to it.
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
