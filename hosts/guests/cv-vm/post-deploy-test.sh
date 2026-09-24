#!/usr/bin/env bash
# Behavioural check of the cv-vm network seal: the running kernel actually drops guest->LAN/host
# while keeping internet egress. Eval only proves the nftables table in modules/nixos/microvm/host.nix
# builds, not that it works, so this is the seal's only real verification. Run after each deploy:
#   ./hosts/guests/cv-vm/post-deploy-test.sh
#
# Probed LAN/host ports must be OPEN when reachable (NAS SMB 445, compute sshd 22) so that a
# "blocked" result unambiguously means the firewall dropped, not a closed port. Needs admin SSH.
set -euo pipefail

jump=compute vm=cv-vm user=bphenriques
fail=0

# want=blocked|open. A DROP makes the connect hang to timeout; a closed port RSTs, and both fail
# the /dev/tcp probe, hence the "port must be open when reachable" requirement above.
check() {
  local host=$1 port=$2 want=$3 desc=$4 got
  if ssh -o BatchMode=yes -o ConnectTimeout=8 -J "$jump" "$user@$vm" \
    "timeout 3 bash -c 'echo > /dev/tcp/$host/$port'" 2>/dev/null; then got=open; else got=blocked; fi
  if [[ $got == "$want" ]]; then
    printf 'PASS  %-22s %s:%-4s -> %s\n' "$desc" "$host" "$port" "$got"
  else
    printf 'FAIL  %-22s %s:%-4s -> %s (want %s)\n' "$desc" "$host" "$port" "$got" "$want"
    fail=1
  fi
}

echo "== cv-vm containment =="
check 192.168.1.199 445 blocked "LAN (NAS SMB)"
check 10.20.1.1 22 blocked "host (compute pivot)"
check 1.1.1.1 443 open "internet egress"

[[ $fail -eq 0 ]] && echo "OK: seal holds." || echo "SEAL BROKEN: cv-vm is continuously exposed, investigate now."
exit $fail
