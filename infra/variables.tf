variable "zone_id" {
  type = string
}

variable "account_id" {
  type = string
}

variable "domain" {
  type = string
}

variable "internal_records" {
  type = map(string) # <sub> -> internal IP
}

variable "redirects" {
  type = map(string) # <sub> -> 302 target URL
}

variable "github_pages_verification" {
  type = string # TXT value from github.com/settings/pages
}
