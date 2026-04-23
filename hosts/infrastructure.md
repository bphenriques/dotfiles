# Cloudflare

Instructions on how to setup Cloudflare. Could be terraformed, but it is overkill for a small set of records.

Assumptions:

- You have a domain registered in a Cloudflare account.
- You have set static IP addresses to the devices. You don't want the internal IPs to keep changing.

## DNS Records

Cloudflare suggests:

> Add an A, AAAA, or CNAME record for www so that www.MYDOMAIN.com will resolve.
> Add an A, AAAA, or CNAME record for your root domain so that MYDOMAIN.com will resolve.
> Add an MX record for your root domain so that mail can reach @MYDOMAIN.com addresses or set up restrictive SPF, DKIM,
> and DMARC records to prevent email spoofing.

| Type  | Name      | Content            | Proxy Status           | TTL  |
| ----- | --------- | ------------------ | ---------------------- | ---- |
| A     | @         | Compute IP         | DNS only - reserved IP | Auto |
| A     | dev       | NAS IP             | DNS only - reserved IP | Auto |
| A     | \*.dev    | NAS IP             | DNS only - reserved IP | Auto |
| A     | local     | Laptop IP          | DNS only - reserved IP | Auto |
| A     | \*.local  | Laptop IP          | DNS only - reserved IP | Auto |
| CNAME | www       | @                  | DNS only               | Auto |
| CNAME | www.local | local.@            | DNS only               | Auto |
| MX    | @         | email.MYDOMAIN.com | -                      | Auto |

Notes:

- `@` means the root domain, therefore it will be `MYDOMAIN.com`.
- The `*.local` and `*.dev` wildcards allow subdomains for local services.
- Added the `MX` record, but I am aware that there is more to it. Later.

## Traefik Integration

Using Traefik as reverse proxy with `DNS-01 Challenge`:

1. Create an [API Token](https://dash.cloudflare.com/profile/api-tokens) with `Edit zone DNS`:
   1. `Edit` permissions to `Zone` + `DNS`.
   2. Include specific Zone: your domain.
2. Export the token.
