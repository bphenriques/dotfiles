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

variable "tunnel" {
  type = object({
    name     = string
    hostname = string
    service  = string
  })
}
