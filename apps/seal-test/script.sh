# shellcheck shell=bash

user=bphenriques
only=${1:-}
current="" fail=0

probe() {
  local host=$1 port=$2
  shift 2
  ssh -n -o BatchMode=yes -o ConnectTimeout=8 "$@" "timeout 3 bash -c 'echo > /dev/tcp/$host/$port'" 2>/dev/null
}

while read -r jump vm host port want label; do
  [ -z "$only" ] || [ "$vm" = "$only" ] || continue
  if [ "$vm" != "$current" ]; then
    current=$vm
    echo "== $vm =="
    # An unreachable guest would score every probe "blocked".
    ssh -n -o BatchMode=yes -o ConnectTimeout=8 -J "$jump" "$user@$vm" true 2>/dev/null \
      || { echo "ABORT: cannot ssh to $vm via $jump" >&2; exit 2; }
  fi
  # Only a target the jump host reaches proves anything: then "blocked" can only mean the seal dropped it.
  if ! probe "$host" "$port" "$jump"; then
    printf 'SKIP  %-26s %s:%s unreachable from %s\n' "$label" "$host" "$port" "$jump"
    continue
  fi
  if probe "$host" "$port" -J "$jump" "$user@$vm"; then got=open; else got=blocked; fi
  if [ "$got" = "$want" ]; then
    printf 'PASS  %-26s %s:%s %s\n' "$label" "$host" "$port" "$got"
  else
    printf 'FAIL  %-26s %s:%s %s, want %s\n' "$label" "$host" "$port" "$got" "$want"
    fail=1
  fi
done <<<"$PROBES"

[ -n "$current" ] || { echo "no guest named $only" >&2; exit 2; }
exit "$fail"
