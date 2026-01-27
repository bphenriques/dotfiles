# Generate OIDC clients

Generates secure OAuth2 client credentials for OIDC client registration.

Why: Both the provider and client read from the same SOPS secret, so credentials must exist before the first deployment.

## Usage

```bash
nix run .#generate-oidc-client
```

Outputs:
```
OAUTH2_CLIENT_ID=<random-hex>
OAUTH2_CLIENT_SECRET=<random-base64>
```

Then add to SOPS secrets (e.g., `secrets/pocket-id/oidc-clients/miniflux.yaml`):
```yaml
pocket-id:
  oidc-clients:
    miniflux:
      OAUTH2_CLIENT_ID: a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4
      OAUTH2_CLIENT_SECRET: xYz123AbCdEfGhIjKlMnOpQrStUvWxYz456789012
```

Once deployed, both new credentials will be used by both the server and client.