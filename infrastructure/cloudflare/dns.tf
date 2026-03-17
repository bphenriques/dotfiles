locals {
  zone_id = data.cloudflare_zone.main.id

  # IPs matching hosts/shared.nix
  ip_compute = "192.168.1.196"
  ip_nas     = "192.168.1.192"
  ip_laptop  = "192.168.1.121"
}

# Root domain -> compute
resource "cloudflare_dns_record" "root" {
  zone_id = local.zone_id
  type    = "A"
  name    = "@"
  content = local.ip_compute
  proxied = false
  ttl     = 1
}

# www -> root domain
resource "cloudflare_dns_record" "www" {
  zone_id = local.zone_id
  type    = "CNAME"
  name    = "www"
  content = var.domain
  proxied = false
  ttl     = 1
}

# dev -> NAS
resource "cloudflare_dns_record" "dev" {
  zone_id = local.zone_id
  type    = "A"
  name    = "dev"
  content = local.ip_nas
  proxied = false
  ttl     = 1
}

# *.dev -> NAS
resource "cloudflare_dns_record" "dev_wildcard" {
  zone_id = local.zone_id
  type    = "A"
  name    = "*.dev"
  content = local.ip_nas
  proxied = false
  ttl     = 1
}

# local -> laptop
resource "cloudflare_dns_record" "local" {
  zone_id = local.zone_id
  type    = "A"
  name    = "local"
  content = local.ip_laptop
  proxied = false
  ttl     = 1
}

# *.local -> laptop
resource "cloudflare_dns_record" "local_wildcard" {
  zone_id = local.zone_id
  type    = "A"
  name    = "*.local"
  content = local.ip_laptop
  proxied = false
  ttl     = 1
}

# www.local -> local.<domain>
resource "cloudflare_dns_record" "www_local" {
  zone_id = local.zone_id
  type    = "CNAME"
  name    = "www.local"
  content = "local.${var.domain}"
  proxied = false
  ttl     = 1
}

# MX record
resource "cloudflare_dns_record" "mx" {
  zone_id  = local.zone_id
  type     = "MX"
  name     = "@"
  content  = "email.${var.domain}"
  ttl      = 1
  priority = 10
}
