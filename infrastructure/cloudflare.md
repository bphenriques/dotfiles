# Cloudflare

Assumptions:
- You have a domain registered in a Cloudflare account.
- You have set static IP addresses to the devices. You don't want the internal IPs to keep changing.

# DNS Records

I suggest applying the suggestions:
> Add an A, AAAA, or CNAME record for www so that www.MYDOMAIN.com will resolve.
> Add an A, AAAA, or CNAME record for your root domain so that MYDOMAIN.com.com will resolve.
> Add an MX record for your root domain so that mail can reach @MYDOMAIN.com addresses or set up restrictive SPF, DKIM, and DMARC records to prevent email spoofing.

| Type  | name      | Content             | Proxy status           | TTL  |
|-------|-----------|---------------------|------------------------|------|
| A     | @         | Synology IP         | DNS only - reserved IP | Auto |
| A     | local     | Development Machine | DNS only - reserved IP | Auto |
| A     | *.local.@ | Development Machine | DNS only - reserved IP | Auto |
| CNAME | www.@     | @                   | DNS only - reserved IP | Auto |
| CNAME | www.local | local.@             | DNS only - reserved IP | Auto |
| MX    | @         | email.MYDOMAIN.com  | -                      | Auto |

Notes:
- `@` means the root domain, therefore it will be `MYDOMAIN.com`.
- The `*.local.@` is limited to my machine but allows me to try things locally before commiting.
- Added the `MX` record, but I am aware that there is more to it. Later.
- Bah, I would like to terraform this.

# Traefik integration

I am using Traefik as reverse proxy, let's configure the `DNS-01 Challenge`:
1. Create a [API Tokens](https://dash.cloudflare.com/profile/api-tokens) then `Edit zone DNS`:
    1. `Edit` permissions to `Zone` + `DNS`.
    2. Include specific Zone: your domain.

Export the token.