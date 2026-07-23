# Cloudflare

Manages my Cloudflare account (DNS, redirects)

## Run

```
nix develop .#infra
tofu init
tofu plan
```

Note: `tofu` is wrapped so the API token and state passphrase are decrypted from sops per invocation. They
never enter the shell env. State is encrypted (OpenTofu native) and stored in a separate repository containing the secrets as well:

```yaml
cloudflare:
  api_token: <token>
  state_passphrase: <openssl rand -hex 32>
```

- **api_token**: Cloudflare token with zone `DNS` Edit, `Single Redirect` Edit, `Zone` Read, plus account `Cloudflare Tunnel` Edit (the tunnel is account-scoped). It has an expiry; when it lapses, mint a new one with the same scopes and replace this value.
- **state_passphrase**: random secret encrypting the state (PBKDF2, AES-GCM). Lose it and the committed state can't be decrypted.