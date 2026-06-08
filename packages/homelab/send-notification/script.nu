#!/usr/bin/env nu
# Default (ntfy) implementation of the homelab notify contract.
# Reads NOTIFY_URL (base url) and NOTIFY_TOKEN_FILE (publisher token) from the environment.
def main [
  --topic: string
  --message: string
  --title: string
  --priority: string   # low | default | high
  --tags: string       # ntfy emoji tags; ignored by other backends
] {
  if ($topic | is-empty) { error make { msg: "--topic is required" } }
  if ($message | is-empty) { error make { msg: "--message is required" } }
  let url = $env.NOTIFY_URL
  let token = open --raw $env.NOTIFY_TOKEN_FILE | str trim
  mut headers = { Authorization: $"Bearer ($token)", Markdown: "yes" }
  if ($title | is-not-empty) { $headers = ($headers | insert Title $title) }
  if ($tags | is-not-empty) { $headers = ($headers | insert Tags $tags) }
  if ($priority | is-not-empty) { $headers = ($headers | insert Priority $priority) }
  http post --headers $headers --content-type text/plain $"($url)/($topic)" $message
}
