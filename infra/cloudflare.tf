# Split-horizon: public DNS -> internal IP, unproxied.
resource "cloudflare_dns_record" "internal" {
  for_each = var.internal_records

  zone_id = var.zone_id
  name    = "${each.key}.${var.domain}"
  type    = "A"
  content = each.value
  proxied = false
  ttl     = 1
}

# Proxied placeholder so the edge redirect can fire; the 192.0.2.1 origin is never reached.
resource "cloudflare_dns_record" "redirect" {
  for_each = var.redirects

  zone_id = var.zone_id
  name    = "${each.key}.${var.domain}"
  type    = "A"
  content = "192.0.2.1"
  proxied = true
  ttl     = 1
}

# Per-zone dynamic-redirect ruleset (a singleton), so TF owns every rule in it.
resource "cloudflare_ruleset" "redirects" {
  zone_id = var.zone_id
  name    = "default"
  kind    = "zone"
  phase   = "http_request_dynamic_redirect"

  rules = [
    for sub, url in var.redirects : {
      description = "${sub} redirect"
      expression  = "http.host eq \"${sub}.${var.domain}\""
      action      = "redirect"
      action_parameters = {
        from_value = {
          preserve_query_string = true
          status_code           = 302
          target_url = {
            value = url
          }
        }
      }
    }
  ]
}

resource "cloudflare_zone_setting" "always_use_https" {
  zone_id    = var.zone_id
  setting_id = "always_use_https"
  value      = "on"
}

# HSTS. include_subdomains stays off so the unproxied internal HTTP records aren't forced to HTTPS.
resource "cloudflare_zone_setting" "security_header" {
  zone_id    = var.zone_id
  setting_id = "security_header"
  value = {
    strict_transport_security = {
      enabled            = true
      max_age            = 31536000
      include_subdomains = false
      preload            = false
      nosniff            = false
    }
  }
}

# Apex landing: GitHub Pages (bphenriques/bphenriques), flattened by Cloudflare. Unproxied so GitHub can issue its certificate.
resource "cloudflare_dns_record" "apex" {
  zone_id = var.zone_id
  name    = var.domain
  type    = "CNAME"
  content = "bphenriques.github.io"
  proxied = false
  ttl     = 1
}

# Verified domain: no other GitHub account can claim it for Pages.
resource "cloudflare_dns_record" "github_pages_verification" {
  zone_id = var.zone_id
  name    = "_github-pages-challenge-bphenriques.${var.domain}"
  type    = "TXT"
  content = "\"${var.github_pages_verification}\""
  ttl     = 1
}
