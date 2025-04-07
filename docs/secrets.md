Depending on the use-case and type of systems, a host might have as secrets:
- A SOPS private key
- LUKS encryption key

## Sops private key

1. Generate key pair using: `nix-shell -p age --command "age-keygen"`.
2. Export the private key to `$HOME/.config/sops/age/keys.txt`
3. Add new host to `.sops.yaml` using the public key to `.sops.yaml` and the correct `path_regex`.

# FIXME: the yaml anchor has to match as we have automations to retrieve that

# FIXME
3. Upload to Bitwarden using the format `sops-age-key-$HOST-$USER` with a `private` field inside.
3. Add new host to `.sops.yaml` using the public key to `.sops.yaml` and the correct `path_regex`.

## LUKS encryption keys
