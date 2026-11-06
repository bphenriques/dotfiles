# shellcheck shell=bash

only=${1:-}
current="" fail=0

reach() {
  local from=$1 host=$2 port=$3
  ssh -n -o BatchMode=yes -o ConnectTimeout=8 "$from" "timeout 3 bash -c 'echo > /dev/tcp/$host/$port'" 2>/dev/null
}

while read -r from control host port want label; do
  [ -z "$only" ] || [ "$from" = "$only" ] || continue
  if [ "$from" != "$current" ]; then
    current=$from
    echo "== $from =="
    # An unreachable source would score every probe "blocked".
    ssh -n -o BatchMode=yes -o ConnectTimeout=8 "$from" true 2>/dev/null \
      || { echo "ABORT: cannot ssh to $from" >&2; exit 2; }
  fi
  if ! reach "$control" "$host" "$port"; then
    printf 'SKIP  %-34s %s:%s unreachable from %s\n' "$label" "$host" "$port" "$control"
    continue
  fi
  if reach "$from" "$host" "$port"; then got=open; else got=blocked; fi
  if [ "$got" = "$want" ]; then
    printf 'PASS  %-34s %s:%s %s\n' "$label" "$host" "$port" "$got"
  else
    printf 'FAIL  %-34s %s:%s %s, want %s\n' "$label" "$host" "$port" "$got" "$want"
    fail=1
  fi
done <<<"$PROBES"

[ -n "$current" ] || { echo "no probes from $only" >&2; exit 2; }
exit "$fail"
