# Admin CLI (fleet *-manage convention). Issue a recipient's password — a 5-word
# passphrase (easy to relay, ~64 bits — uncrackable for online auth), bcrypt-hashed
# into the htpasswd and printed once; never stored in plaintext:
#   share-manage rotate <user>
def "main rotate" [user: string] {
  let users = $env.SHARE_SCOPES | split row ' '
  if not ($user in $users) {
    print $"unknown user '($user)' — known: ($users | str join ', ')"
    exit 1
  }
  let pw = (^xkcdpass -n 5 -d - | str trim)
  ^htpasswd -bB $env.HTPASSWD $user $pw | ignore
  ^chown traefik:traefik $env.HTPASSWD
  ^chmod 640 $env.HTPASSWD
  # Traefik reads the htpasswd only at startup (it does not watch the file), so the new
  # hash takes effect only after a restart — a brief Funnel blip, fine for an admin action.
  ^systemctl restart traefik
  print $pw
}

def funnel-on [] { ^tailscale funnel --bg --proxy-protocol=2 --tls-terminated-tcp=443 $"tcp://127.0.0.1:($env.FUNNEL_PORT)" }
def funnel-off [] { ^tailscale funnel reset }

# Toggle the public Funnel endpoint. `auto` enforces the nightly dark window and is what
# boot + the schedule timer call; on/off are the manual override.
def "main funnel" [action: string] {
  match $action {
    "on" => { funnel-on }
    "off" => { funnel-off }
    "auto" => {
      let now = (date now | format date "%H%M" | into int)
      if ($now >= ($env.DARK_START | into int) and $now < ($env.DARK_END | into int)) { funnel-off } else { funnel-on }
    }
    "status" => { ^tailscale funnel status }
    _ => {
      print "usage: share-manage funnel on|off|auto|status"
      exit 1
    }
  }
}

def main [] {
  print "usage: share-manage rotate <scope> | funnel on|off|status"
  exit 1
}
