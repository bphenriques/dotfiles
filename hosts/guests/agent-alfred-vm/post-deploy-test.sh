#!/usr/bin/env bash
# Behavioural check of the agent-alfred-vm network seal. This guest has two deliberate holes
# (egress.allowLan -> ai:11434, egress.allowHostPorts -> the bridge address on 2222 for gitea's SSH),
# so the test asserts each hole is open AND still scoped to its port and address.
# Run after each deploy:
#   ./hosts/guests/agent-alfred-vm/post-deploy-test.sh
#
# Probed ports must be OPEN when reachable (NAS SMB 445, compute sshd 22 and gitea 2222, ai sshd 22) so
# that a "blocked" result unambiguously means the firewall dropped, not a closed port. Needs admin SSH.
set -euo pipefail

jump=compute vm=agent-alfred-vm user=bphenriques ai=192.168.1.200 compute=192.168.1.196 bridge=10.20.1.1
fail=0

# want=blocked|open. A DROP hangs to timeout; a closed port RSTs, and both fail the /dev/tcp
# probe, hence the "port must be open when reachable" requirement above.
check() {
  local host=$1 port=$2 want=$3 desc=$4 got
  if ssh -o BatchMode=yes -o ConnectTimeout=8 -J "$jump" "$user@$vm" \
    "timeout 3 bash -c 'echo > /dev/tcp/$host/$port'" 2>/dev/null; then got=open; else got=blocked; fi
  if [[ $got == "$want" ]]; then
    printf 'PASS  %-22s %s:%-5s -> %s\n' "$desc" "$host" "$port" "$got"
  else
    printf 'FAIL  %-22s %s:%-5s -> %s (want %s)\n' "$desc" "$host" "$port" "$got" "$want"
    fail=1
  fi
}

# check() scores a failed ssh as "blocked", so an unreachable guest would pass every blocked check.
ssh -o BatchMode=yes -o ConnectTimeout=8 -J "$jump" "$user@$vm" true 2>/dev/null \
  || { echo "ABORT: cannot ssh to $vm via $jump; nothing was tested."; exit 2; }

echo "== agent-alfred-vm containment =="
check 192.168.1.199 445 blocked "LAN (NAS SMB)"
check "$bridge" 22 blocked "host (compute pivot)"
check "$bridge" 2222 open "gitea SSH (the hole)"
check "$compute" 2222 blocked "gitea on LAN address"
check 1.1.1.1 443 open "internet egress"

# ai sleeps when idle; the gate doubles as ground truth that its :22 is genuinely open.
if ssh -o BatchMode=yes -o ConnectTimeout=8 "$jump" "timeout 3 bash -c 'echo > /dev/tcp/$ai/22'" 2>/dev/null; then
  check "$ai" 11434 open "ai Ollama (the hole)"
  check "$ai" 22 blocked "ai, other ports"
else
  printf 'SKIP  %-22s ai is down; wake it to test the egress hole\n' "ai egress hole"
fi

[[ $fail -eq 0 ]] && echo "OK: seal holds." || echo "SEAL BROKEN: investigate."
exit $fail
