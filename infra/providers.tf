terraform {
  required_providers {
    cloudflare = {
      source = "cloudflare/cloudflare"
    }
  }
  backend "local" {} # path -> encrypted state in dotfiles-private, passed at `tofu init`
}

provider "cloudflare" {} # CLOUDFLARE_API_TOKEN is injected by the devShell
