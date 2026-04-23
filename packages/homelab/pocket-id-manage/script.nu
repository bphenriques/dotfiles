#!/usr/bin/env nu
# Pocket-ID management CLI.
#
# Subcommands:
#   provision-users    Provision Nix-managed users and groups (systemd)
#   provision-client   Provision a single OIDC client with credentials (systemd)
#   invite             Create a guest user and send invite email (manual)
#   reinvite           Resend one-time access email (manual)
#   list               List all users with groups (manual)
#   remove             Remove CLI-managed guest users (manual)
# Good enough for homelab scale; Pocket-ID unlikely to exceed 100 users/groups/clients.
const PAGINATION_LIMIT = 100
let base_url = $env.POCKET_ID_URL
let api_key = open $env.POCKET_ID_API_KEY_FILE | str trim
let headers = {"X-API-KEY": $api_key}
# --- Shared helpers ---
def wait_ready [] {
  for attempt in 1..60 {
    print $"Waiting for Pocket ID... ($attempt)"
    try {
      http get $"($base_url)/.well-known/openid-configuration" --max-time 2sec | ignore
      return
    } catch { sleep 2sec }
  }
  error make {msg: "Pocket ID failed to start in time"}
}
def get_all [resource: string] {
  let r = http get $"($base_url)/api/($resource)?pagination[limit]=($PAGINATION_LIMIT)" --headers $headers --full --allow-errors
  if $r.status != 200 {
    error make {
      msg: $"Failed to list ($resource): ($r.status) - ($r.body)"
    }
  }
  $r.body.data
}
def find_user [username: string] {
  let user = get_all "users" | where username == $username | get 0?
  if $user == null {
    error make {
      msg: $"User '($username)' not found"
    }
  }
  $user
}
def find_group [name: string] {
  let group = get_all "user-groups" | where name == $name | get 0?
  if $group == null {
    error make {
      msg: $"Group '($name)' not found"
    }
  }
  $group
}
def send_invite [user_id: string, email: string] {
  let r = http post $"($base_url)/api/users/($user_id)/one-time-access-email" "{}" --headers $headers --content-type application/json --full --allow-errors
  if $r.status == 204 {
    print $"Sent invite email to ($email)"
  } else {
    error make {
      msg: $"Failed to send invite email to ($email): ($r.status) - ($r.body)"
    }
  }
}
def write_credential [path: string, content: string, group: string] {
  let tmp = $"($path).tmp"
  $content | save --force $tmp
  chmod 0640 $tmp
  chown $"root:($group)" $tmp
  mv --force $tmp $path # atomic rename
}
def resolve_group_ids [group_names: list<string>] {
  let existing = get_all "user-groups"
  $group_names | each { |g|
    let found = $existing | where name == $g | get 0?
    if $found == null { error make { msg: $"Group '($g)' not found in Pocket-ID" } }
    $found.id
  }
}
# --- Systemd: provision Nix-managed users and groups ---
def "main provision-users" [] {
  let config = open $env.OIDC_CONFIG_FILE
  let credentials_dir = $env.OIDC_CREDENTIALS_DIR
  wait_ready
  print "Pocket ID is ready"
  # Provision groups
  print "Provisioning groups..."
  let existing_groups = get_all "user-groups"
  let group_body = {|g| {
    name: $g.name
    friendlyName: $g.name
  } }
  let group_map = $config.groups | each { |g|
      let found = $existing_groups | where name == $g.name | get 0?
      if $found != null {
        let r = http put $"($base_url)/api/user-groups/($found.id)" (do $group_body $g) --headers $headers --content-type application/json --full --allow-errors
        if $r.status != 200 { error make { msg: $"Failed to update group ($g.name): ($r.status) - ($r.body)" } }
        return [$g.name $found.id]
      }

      let r = http post $"($base_url)/api/user-groups" (do $group_body $g) --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 201 { error make { msg: $"Failed to create group ($g.name): ($r.status) - ($r.body)" } }
      print $"  Created group: ($g.name)"
      [$g.name $r.body.id]
    } | into record
  # Provision users
  print "Provisioning users..."
  let existing_users = get_all "users"
  let provisioned_users = $config.users | each { |u|
    print $"Processing user: ($u.username)"
    let group_ids = $u.groups | each { |g| $group_map | get $g }

    let body = {
      username: $u.username
      email: $u.email
      emailVerified: true
      firstName: $u.firstName
      lastName: $u.lastName
      displayName: $"($u.firstName) ($u.lastName)"
      isAdmin: $u.isAdmin
      customClaims: [{ key: "managed-by", value: "nix" }]
    }

    let found = $existing_users | where username == $u.username | get 0?
    let user_id = if $found != null {
      let r = http put $"($base_url)/api/users/($found.id)" $body --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 200 { error make { msg: $"Failed to update user ($u.username): ($r.status) - ($r.body)" } }
      $found.id
    } else {
      let r = http post $"($base_url)/api/users" $body --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 201 { error make { msg: $"Failed to create user ($u.username): ($r.status) - ($r.body)" } }
      print $"  Created user: ($u.username)"
      send_invite $r.body.id $u.email
      $r.body.id
    }

    let gr = http put $"($base_url)/api/users/($user_id)/user-groups" { userGroupIds: $group_ids } --headers $headers --content-type application/json --full --allow-errors
    if $gr.status != 200 { error make { msg: $"Failed to set groups for ($u.username): ($gr.status) - ($gr.body)" } }

    { username: $u.username, id: $user_id }
  }
  # Write users mapping file
  let users_file = $"($credentials_dir)/oidc-users.json"
  let users_tmp = $"($users_file).tmp"
  $provisioned_users | to json | save --force $users_tmp
  chmod 0644 $users_tmp
  mv --force $users_tmp $users_file # atomic rename
  print $"Wrote users mapping to ($users_file)"
  print "Pocket ID base provisioning complete"
}
# --- Systemd: provision a single OIDC client ---
def "main provision-client" [] {
  let client = open $env.OIDC_CLIENT_CONFIG_FILE
  let credentials_dir = $env.OIDC_CREDENTIALS_DIR
  wait_ready
  let client_dir = $"($credentials_dir)/($client.name)"
  let client_group = $"homelab-oidc-($client.name)"
  let id_file = $"($client_dir)/id"
  let secret_file = $"($client_dir)/secret"
  print $"Provisioning OIDC client: ($client.name)"
  let existing_clients = get_all "oidc/clients"
  let found = $existing_clients | where name == $client.name | get 0?
  let allowed_groups = $client.allowedGroups? | default []
  let is_group_restricted = ($allowed_groups | is-not-empty)
  let desired = {
    name: $client.name
    callbackURLs: ($client.callbackURLs | sort)
    pkceEnabled: $client.pkce
    isPublic: $client.pkce
    isGroupRestricted: $is_group_restricted
  }
  let is_new = $found == null
  let client_id = if $found != null {
    let dominated_fields = $desired | columns
    let current = $found | select ...$dominated_fields | update callbackURLs { sort }
    if $current != $desired {
      let r = http put $"($base_url)/api/oidc/clients/($found.id)" $desired --headers $headers --content-type application/json --full --allow-errors
      if $r.status != 200 {
        error make {
          msg: $"Failed to update OIDC client ($client.name): ($r.status) - ($r.body)"
        }
      }
      print $"  Updated OIDC client config: ($client.name)"
    } else {
      print $"  OIDC client config up-to-date: ($client.name)"
    }
    $found.id
  } else {
    let r = http post $"($base_url)/api/oidc/clients" $desired --headers $headers --content-type application/json --full --allow-errors
    if $r.status != 201 {
      error make {
        msg: $"Failed to create OIDC client ($client.name): ($r.status) - ($r.body)"
      }
    }
    print $"  Created OIDC client: ($client.name)"
    $r.body.id
  }
  # Detect if local credentials are missing or stale (id mismatch means server-side recreate)
  let local_id = if ($id_file | path exists) {
    open $id_file | str trim
  } else { null }
  let credentials_missing = not ($id_file | path exists) or not ($secret_file | path exists)
  let id_mismatch = $local_id != null and $local_id != $client_id
  let needs_secret = $is_new or $credentials_missing or $id_mismatch
  if $id_mismatch { print $"  Local id mismatch, forcing secret rotation for: ($client.name)" }
  # Generate new secret only when needed
  let client_secret = if $needs_secret {
    let secret_r = http post $"($base_url)/api/oidc/clients/($client_id)/secret" "" --headers $headers --content-type application/json --full --allow-errors
    if $secret_r.status != 200 {
      error make {
        msg: $"Failed to generate secret for OIDC client ($client.name): ($secret_r.status) - ($secret_r.body)"
      }
    }
    print $"  Generated new secret for: ($client.name)"
    $secret_r.body.secret
  } else {
    print $"  Secret exists, skipping rotation for: ($client.name)"
    null
  }
  # Write credentials
  if not ($client_dir | path exists) { mkdir $client_dir }
  chmod 0750 $client_dir
  chown $"root:($client_group)" $client_dir
  if $needs_secret {
    write_credential $id_file ($client_id | into string) $client_group
    write_credential $secret_file $client_secret $client_group
  }
  print $"  Credentials ready at ($client_dir)"
  # Update allowed user groups if restricted
  if $is_group_restricted {
    let group_ids = resolve_group_ids $allowed_groups
    let gr = http put $"($base_url)/api/oidc/clients/($client_id)/allowed-user-groups" { userGroupIds: $group_ids } --headers $headers --content-type application/json --full --allow-errors
    if $gr.status != 200 {
      error make {
        msg: $"Failed to set allowed groups for ($client.name): ($gr.status) - ($gr.body)"
      }
    }
    print $"  Set allowed groups: ($allowed_groups | str join ', ')"
  }
  print $"OIDC client ($client.name) provisioning complete"
}
# --- Manual: guest management ---
# Create a guest user, assign to guests group, and send invite email.
def "main guest invite" [
  email: string        # Email address
  --firstName: string  # First name
  --lastName: string   # Last name
  --username: string   # Username (defaults to email local part)
] {
  let guests_group = $env.POCKET_ID_GUESTS_GROUP
  let uname = if $username != null { $username } else {
    $email | split row "@" | get 0
  }
  let fname = if $firstName != null { $firstName } else { $uname }
  let lname = if $lastName != null { $lastName } else { "" }
  let existing = get_all "users"
  let found = $existing | where username == $uname | get 0?
  if $found != null {
    error make {
      msg: $"User '($uname)' already exists. Use 'reinvite' to resend the invite."
    }
  }
  let body = {
    username: $uname
    email: $email
    emailVerified: true
    firstName: $fname
    lastName: $lname
    displayName: ([$fname, $lname] | where { $in != "" } | str join " ")
    isAdmin: false
  }
  let r = http post $"($base_url)/api/users" $body --headers $headers --content-type application/json --full --allow-errors
  if $r.status != 201 {
    error make {
      msg: $"Failed to create user ($uname): ($r.status) - ($r.body)"
    }
  }
  let user_id = $r.body.id
  print $"Created user: ($uname)"
  # Assign to guests group
  let group = find_group $guests_group
  let gr = http put $"($base_url)/api/users/($user_id)/user-groups" { userGroupIds: [$group.id] } --headers $headers --content-type application/json --full --allow-errors
  if $gr.status != 200 {
    error make {
      msg: $"Failed to assign group: ($gr.status) - ($gr.body)"
    }
  }
  print $"Assigned to group: ($guests_group)"
  send_invite $user_id $email
}
# Remove guest users. Only removes users in the guests group.
def "main guest remove" [...usernames: string] {
  let guests_group = $env.POCKET_ID_GUESTS_GROUP
  let all_users = get_all "users"
  let guest_group = find_group $guests_group
  for uname in $usernames {
    let user = $all_users | where username == $uname | get 0?
    if $user == null {
      print $"User '($uname)' not found, skipping"
      continue
    }
    let user_group_ids = $user.userGroups? | default [] | get id
    if $guest_group.id not-in $user_group_ids {
      print $"User '($uname)' is not a guest, skipping."
      continue
    }
    let r = http delete $"($base_url)/api/users/($user.id)" --headers $headers --full --allow-errors
    if $r.status != 204 and $r.status != 200 {
      print $"Failed to remove ($uname): ($r.status) - ($r.body)"
    } else {
      print $"Removed guest: ($uname)"
    }
  }
}
# Resend one-time access email to any user.
def "main reinvite" [username: string] {
  let user = find_user $username
  send_invite $user.id $user.email
}
# List all users with their groups.
def "main list" [] {
  let users = get_all "users"
  if ($users | is-empty) {
    print "No users"
    return
  }
  let groups = get_all "user-groups"
  $users | each { |u|
    let user_groups = $u.userGroups? | default [] | each { |ug|
      let g = $groups | where id == $ug.id | get 0?
      if $g != null { $g.name } else { $ug.id }
    }
    {
      username: $u.username
      email: $u.email
      groups: ($user_groups | str join ", ")
    }
  }
}
def main [] { print "pocket-id-manage - Pocket-ID management

  Provisioning (systemd):
    provision-users          Provision Nix-managed users and groups
    provision-client         Provision a single OIDC client with credentials

  Guest management (manual):
    guest invite <email> [--firstName] [--lastName] [--username]
                             Create guest user and send invite email
    guest remove <username>  Remove guest users

  General (manual):
    reinvite <username>      Resend one-time access email
    list                     List all users with groups" }
