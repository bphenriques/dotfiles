#!/usr/bin/env bash
# Behavioural check of the agent-vm network seal. This guest has one deliberate hole
# (egress.allowLan -> ai:11434), so the test asserts the hole is open AND still port-scoped.
# Run after each deploy:
#   ./hosts/guests/agent-vm/post-deploy-test.sh
#
# Probed ports must be OPEN when reachable (NAS SMB 445, compute sshd 22, ai sshd 22) so that a
# "blocked" result unambiguously means the firewall dropped, not a closed port. Needs admin SSH.
set -euo pipefail

jump=compute vm=agent-vm user=bphenriques ai=192.168.1.200
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

echo "== agent-vm containment =="
check 192.168.1.199 445 blocked "LAN (NAS SMB)"
check 10.20.1.1 22 blocked "host (compute pivot)"
check 1.1.1.1 443 open "internet egress"

# ai sleeps when idle; the gate doubles as ground truth that its :22 is genuinely open.
if ssh -o BatchMode=yes -o ConnectTimeout=8 "$jump" "timeout 3 bash -c 'echo > /dev/tcp/$ai/22'" 2>/dev/null; then
  check "$ai" 11434 open    "ai Ollama (the hole)"
  check "$ai" 22    blocked "ai, other ports"
else
  printf 'SKIP  %-22s ai is down; wake it to test the egress hole\n' "ai egress hole"
fi

[[ $fail -eq 0 ]] && echo "OK: seal holds." || echo "SEAL BROKEN: investigate."
exit $fail
