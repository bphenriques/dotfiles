terraform {
  required_version = ">= 1.10"

  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 5.18.0"
    }
  }

  backend "local" {}
}

provider "cloudflare" {
  # Reads CLOUDFLARE_API_TOKEN from environment (injected by apply.sh via sops)
}

data "cloudflare_zone" "main" {
  filter = {
    name = var.domain
  }
}
