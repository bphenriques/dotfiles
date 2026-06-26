# shellcheck shell=bash
# After a microvm guest's first boot, capture its SSH host key and print the two
# registrations it needs: the known-hosts pin (hosts/shared.nix) and the sops recipient
# (dotfiles-private/.sops.yaml). Run from the repo root: nix run .#host-keys <guest>

[[ $# -eq 1 ]] || {
  echo "usage: host-keys <microvm-guest>   (run after its first boot)" >&2
  exit 1
}
host="$1"

vmip=$(nix eval --raw --impure --expr "(import ./hosts/shared.nix).microvm.hosts.\"${host}\"" 2>/dev/null) \
  || {
    echo "error: '${host}' is not a microvm guest in hosts/shared.nix" >&2
    exit 1
  }

echo "reading ${host} (${vmip}) host key via compute…" >&2
# vmip is a local value, intentionally expanded here before running on compute
# shellcheck disable=SC2029
sshkey=$(ssh compute "ssh-keyscan -t ed25519 ${vmip} 2>/dev/null" | grep -m1 'ssh-ed25519' | awk '{print $2" "$3}')
[[ -n ${sshkey} ]] || {
  echo "error: no key returned — is ${host} booted?" >&2
  exit 1
}
agekey=$(printf '%s\n' "${sshkey}" | ssh-to-age)

cat <<EOF

${host} host key captured. Register it in both places, then commit + redeploy:

  1. dotfiles/hosts/shared.nix  →  hostKeys:
       ${host} = "${sshkey}";

  2. dotfiles-private/.sops.yaml  →  keys (the guest's OWN recipient; keep &base-microvm):
       - &${host} ${agekey}
     add &${host} to the ${host} key_group under creation_rules, then re-encrypt:
       sops updatekeys hosts/${host}/secrets.yaml
EOF
