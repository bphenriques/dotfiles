# Cloudflare

Manages my Cloudflare account (DNS, redirects)

## Run

```
nix develop .#infra
tofu init
tofu plan
```

When a nixpkgs bump changes the bundled provider, re-lock with `tofu init -upgrade`.

Note: `tofu` is wrapped so the API token and state passphrase are decrypted from sops per invocation. They
never enter the shell env. State is encrypted (OpenTofu native) and stored in a separate repository containing the secrets as well:

```yaml
cloudflare:
  api_token: <token>
  state_passphrase: <openssl rand -hex 32>
```

- **api_token**: Cloudflare token with zone `DNS` Edit, `Single Redirect` Edit, `Zone` Read. It has an expiry; when it lapses, mint a new one with the same scopes and replace this value.
- **state_passphrase**: random secret encrypting the state (PBKDF2, AES-GCM). Lose it and the committed state can't be decrypted.

## Apex (GitHub Pages)

The apex is a CNAME to `bphenriques.github.io` (flattened by Cloudflare) serving [bphenriques/bphenriques](https://github.com/bphenriques/bphenriques). One-time setup:

1. [github.com/settings/pages](https://github.com/settings/pages) → Add a domain. Put the TXT value in `github_pages_verification` (`dotfiles-private/infra/settings.nix`).
2. `tofu apply`, then Verify.
3. Repo Settings → Pages → Custom domain. The DNS check and certificate take minutes to an hour, and the site shows a certificate error until then. Then tick Enforce HTTPS. If the check is stuck, remove and re-add the domain.

The fleet section of the page reads [`fleet.json`](../fleet.json). After a fleet change, `nix run .#fleet-json`, commit, push, then run that repo's Pages workflow by hand.
