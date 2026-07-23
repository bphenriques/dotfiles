terraform {
  required_providers {
    cloudflare = {
      source = "cloudflare/cloudflare"
    }
  }
  backend "local" {} # path -> encrypted state in dotfiles-private, passed at `tofu init`
}

provider "cloudflare" {} # CLOUDFLARE_API_TOKEN is injected by the devShell

variable "zone_id" {
  type = string
}

variable "domain" {
  type = string
}

variable "internal_records" {
  type = map(string) # <sub> -> internal IP
}

variable "funnel_redirects" {
  type = map(string) # <sub> -> Tailscale Funnel URL
}

# Split-horizon A records: <sub>.<domain> -> internal IP, unproxied.
resource "cloudflare_dns_record" "internal" {
  for_each = var.internal_records

  zone_id = var.zone_id
  name    = "${each.key}.${var.domain}"
  type    = "A"
  content = each.value
  proxied = false
  ttl     = 1
}

# Proxied placeholder so the edge redirect can fire — the 192.0.2.1 origin is never reached.
resource "cloudflare_dns_record" "funnel" {
  for_each = var.funnel_redirects

  zone_id = var.zone_id
  name    = "${each.key}.${var.domain}"
  type    = "A"
  content = "192.0.2.1"
  proxied = true
  ttl     = 1
}

# One per-zone dynamic-redirect ruleset: 302 each funnel host to its Funnel URL.
resource "cloudflare_ruleset" "redirects" {
  zone_id = var.zone_id
  name    = "default"
  kind    = "zone"
  phase   = "http_request_dynamic_redirect"

  rules = [
    for sub, url in var.funnel_redirects : {
      description = "${sub} to tailscale"
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
