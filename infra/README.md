# Cloudflare

OpenTofu-managed DNS + redirects for the fleet domain. Config (`dotfiles-private/infra/settings.nix`)
and secrets (`.../infra/secrets.yaml`) live in `dotfiles-private`, read at runtime; this directory
carries only the (data-free) HCL.

## Run

```
nix develop .#infra
tofu init
tofu plan
```

`tofu` is wrapped so the API token and state passphrase are decrypted from sops per invocation —
they never enter the shell env. State is encrypted (OpenTofu native) and committed, encrypted, to
`dotfiles-private/infra/terraform.tfstate`.

## Secrets

`dotfiles-private/infra/secrets.yaml` (sops):

```yaml
cloudflare:
  api_token: <token>
  state_passphrase: <openssl rand -hex 32>
```

- **api_token** — Cloudflare token scoped to the zone only: `DNS` Edit, `Single Redirect` Edit,
  `Zone` Read. **It has an expiry** — when it lapses, mint a new one with the same scopes and
  replace this value (nothing else changes).
- **state_passphrase** — random secret encrypting the state (PBKDF2 → AES-GCM). Lose it and the
  committed state can't be decrypted.

## What it manages

- `internal_records`: `<sub>.<domain>` -> internal IP (unproxied, split-horizon).
- `funnel_redirects`: proxied placeholder + 302 redirect `<sub>.<domain>` -> the VM's Tailscale
  Funnel URL.

TF owns the zone's entire dynamic-redirect ruleset (a per-zone singleton): a redirect added in the
dashboard but absent here is removed on `apply`.

## Not managed here

Deliberately left in the Cloudflare dashboard:

- **Email** — the apex `MX` and SPF/DMARC `TXT` records. Set-and-forget and high-stakes; kept out
  to avoid apply-risk.
- **ACME challenges** — `_acme-challenge.*` `TXT` records are created and removed dynamically by
  Traefik (lego) during certificate issuance. Never import these.

## Bootstrap (one-time import)

The internal records and the `share` redirect already exist in Cloudflare. Import them so the first
plan is a no-op instead of a recreate:

```
tofu import 'cloudflare_dns_record.internal["<sub>"]' <zone_id>/<record_id>
tofu import 'cloudflare_dns_record.funnel["share"]'   <zone_id>/<record_id>
tofu import cloudflare_ruleset.redirects              zones/<zone_id>/<ruleset_id>
```

Record IDs come from the Cloudflare dashboard or the API. HCL resource attributes target the
`cloudflare/cloudflare` v5 provider — verify against the nix-pinned version on the first `plan`.
