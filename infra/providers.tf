terraform {
  required_providers {
    cloudflare = {
      source = "cloudflare/cloudflare"
    }
  }
  backend "local" {}
}

provider "cloudflare" {} # CLOUDFLARE_API_TOKEN is injected
