# shellcheck shell=bash

# Both EC fan tables hold 7 records of [temp, step-down temp, duty%]: fan 1 at 0x11, fan 2 at 0x31.
# Only the two records below 45C set the idle floor; everything from 55C up is left stock.
ec=/sys/kernel/debug/ec/ec0/io
bases=(17 49)
expect=(25 45)

duty=${1:-$IDLE_DUTY}

peek() { od -An -v -t u1 -j "$1" -N1 "$ec" | tr -d ' '; }
poke() { awk -v duty="$2" 'BEGIN { printf "%c", duty }' | dd of="$ec" bs=1 seek="$1" count=1 conv=notrunc status=none; }

# An EC firmware bump could reshuffle the tables, so vet every record before writing any of them.
for base in "${bases[@]}"; do
  for rec in 0 1; do
    got=$(peek "$((base + rec * 3))")
    [ "$got" = "${expect[rec]}" ] || {
      echo "EC record at $((base + rec * 3)) reads ${got}C, expected ${expect[rec]}C" >&2
      exit 1
    }
  done
done

for base in "${bases[@]}"; do
  for rec in 0 1; do
    poke "$((base + rec * 3 + 2))" "$duty"
  done
done

echo "fan duty below 45C set to ${duty}%; the stock 20% returns on power cycle"
