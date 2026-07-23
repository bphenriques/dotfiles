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

moved {
  from = cloudflare_dns_record.funnel
  to   = cloudflare_dns_record.redirect
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

# Apex landing: a Cloudflare Tunnel (config_src = cloudflare -> ingress managed here) + apex CNAME.
resource "cloudflare_zero_trust_tunnel_cloudflared" "cv" {
  account_id = var.account_id
  name       = var.tunnel.name
  config_src = "cloudflare"
}

resource "cloudflare_zero_trust_tunnel_cloudflared_config" "cv" {
  account_id = var.account_id
  tunnel_id  = cloudflare_zero_trust_tunnel_cloudflared.cv.id

  config = {
    ingress = [
      {
        hostname = var.tunnel.hostname
        service  = var.tunnel.service
      },
      {
        service = "http_status:404"
      },
    ]
  }
}

resource "cloudflare_dns_record" "apex" {
  zone_id = var.zone_id
  name    = var.domain
  type    = "CNAME"
  content = "${cloudflare_zero_trust_tunnel_cloudflared.cv.id}.cfargotunnel.com"
  proxied = true
  ttl     = 1
}

data "cloudflare_zero_trust_tunnel_cloudflared_token" "cv" {
  account_id = var.account_id
  tunnel_id  = cloudflare_zero_trust_tunnel_cloudflared.cv.id
}

output "tunnel_token" {
  value     = data.cloudflare_zero_trust_tunnel_cloudflared_token.cv.token
  sensitive = true
}
