#!/usr/bin/env nu

# Resends one-time access email to a user for passkey setup.
# Usage: pocket-id-invite.nu <username>
const PAGINATION_LIMIT = 20

let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let headers = { "X-API-KEY": $api_key, "Content-Type": "application/json" }

def main [username: string] {
  let r = http get $"($base_url)/api/users?pagination[limit]=($PAGINATION_LIMIT)" --headers $headers --full --allow-errors
  if $r.status != 200 { error make { msg: $"Failed to list users: ($r.status) - ($r.body)" } }

  let user = $r.body.data | where username == $username | get 0?
  if $user == null { error make { msg: $"User ($username) not found" } }

  let ir = http post $"($base_url)/api/users/($user.id)/one-time-access-email" "{}" --headers $headers --full --allow-errors
  if $ir.status == 204 {
    print $"Sent invite email to ($user.email)"
  } else {
    print $"Failed to send invite email: ($ir.status) - ($ir.body)"
  }
}
