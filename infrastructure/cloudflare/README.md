# Cloudflare DNS

Terraform project managing Cloudflare DNS records.

State and secrets are stored on the NAS under `INFRASTRUCTURE_BASE_DIR` (default: `/mnt/homelab-bphenriques/infrastructure`):
- `terraform.tfstate` — Terraform state.
- `secrets.yaml` — Sops-encrypted file with `CLOUDFLARE_API_TOKEN` and `TF_VAR_domain`.

## Prerequisites

1. Create a [Cloudflare API Token](https://dash.cloudflare.com/profile/api-tokens) with `Zone DNS Edit` and `Zone Zone Read` permissions.
2. Mount the NAS Samba share at `/mnt/homelab-bphenriques`.
3. Create and encrypt secrets: `sops ${INFRASTRUCTURE_BASE_DIR}/secrets.yaml`.

## Usage

```bash
./infrastructure/cloudflare/apply.sh init
./infrastructure/cloudflare/apply.sh plan
./infrastructure/cloudflare/apply.sh apply
```

## Importing Existing Records

On first run, import each existing DNS record:

```bash
./infrastructure/cloudflare/apply.sh import 'cloudflare_dns_record.root' '<zone_id>/<record_id>'
```

List record IDs via the Cloudflare API:

```bash
curl -s "https://api.cloudflare.com/client/v4/zones/<zone_id>/dns_records" \
  -H "Authorization: Bearer <token>" | jq '.result[] | {id, name, type, content}'
```
