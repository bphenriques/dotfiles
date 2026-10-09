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
