#!/usr/bin/env nu

# Resends one-time access email to a user for passkey setup.
# Usage: pocket-id-invite.nu <username>
const PAGINATION_LIMIT = 20

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim

def api [method: string, endpoint: string, body?: record] {
  let url = $"($base_url)/api($endpoint)"
  let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }
  if $body != null {
    http $method $url $body --headers $headers --full --allow-errors
  } else {
    http $method $url --headers $headers --full --allow-errors
  }
}

def main [username: string] {
  let r = api "get" $"/users?pagination[limit]=($PAGINATION_LIMIT)"
  if $r.status != 200 { error make { msg: $"Failed to list users: ($r.status) - ($r.body)" } }

  let user = $r.body.data | where username == $username | first --skip-empty
  if $user == null { error make { msg: $"User ($username) not found" } }

  let ir = api "post" $"/users/($user.id)/one-time-access-email" {}
  if $ir.status == 204 { print $"Sent invite email to ($user.email)" } 
  else { error make { msg: $"Failed to send invite email: ($ir.status) - ($ir.body)" } }
}
