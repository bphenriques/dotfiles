# Syncs the calendars listed in a YAML file and writes the events desktop-shell's month shows, in its format
# (../README.md#menu). vdirsyncer downloads them and khal expands repeating events; both are configured here, next to
# the events file, so nothing else of theirs is needed.
#
#   desktop-shell-calendar-sync ~/.config/desktop-shell/calendars.yaml ~/.local/state/calendar/events.json
#
# where the YAML is a list of { name, url, color, basicAuth? }, as calendars.yaml here describes. The events file also
# says when a sync is under way and why the last one failed, keeping the events it had.

# ISO throughout, so khal's output is read without parsing dates.
const KHAL_LOCALE = "[locale]
dateformat = %Y-%m-%d
longdateformat = %Y-%m-%d
timeformat = %H:%M
datetimeformat = %Y-%m-%dT%H:%M
longdatetimeformat = %Y-%m-%dT%H:%M"

def is-record [value]: nothing -> bool {
  ($value | describe) =~ '^record'
}

def is-string [value]: nothing -> bool {
  ($value | describe) == "string"
}

def valid-auth [auth]: nothing -> bool {
  (is-record $auth) and (is-string $auth.username?) and ((is-string $auth.password?) != (is-string $auth.passwordFile?))
}

def read-calendars [file: path]: nothing -> list {
  let name = $file | path basename
  let calendars = if ($file | path exists) { open --raw $file | from yaml | default [] } else { [] }
  if not (($calendars | describe) =~ '^(list|table)') {
    error make --unspanned { msg: $"($name): expected a list of calendars" }
  }
  # Each problem by entry and field, never the value: an address or a password is secret.
  let problems = $calendars | enumerate | each {|it|
    let c = if (is-record $it.item) { $it.item } else { {} }
    let auth = $c.basicAuth?
    [
      (if not ((is-string $c.name?) and $c.name =~ '^[A-Za-z0-9_-]+$') { "name must be letters, digits, - or _" })
      (if not (is-string $c.url?) { "url is missing" })
      (if not ((is-string $c.color?) and $c.color =~ '^#[0-9a-fA-F]{6}$') { "color must be quoted \"#rrggbb\"" })
      (if not ($auth == null or (valid-auth $auth)) { "basicAuth needs a username, and a password or a passwordFile" })
    ] | compact | each {|problem| $"entry ($it.index + 1): ($problem)" }
  } | flatten
  let repeated = $calendars | each {|c| if (is-record $c) { $c.name? } } | compact | uniq --repeated
  let problems = $problems | append ($repeated | each {|name| $"name '($name)' is used twice" })
  if ($problems | is-not-empty) {
    error make --unspanned { msg: ($problems | prepend $"($name):" | str join "\n  ") }
  }
  $calendars
}

# The remote's login, if any; a passwordFile is read at each sync, so it can be a secret managed elsewhere.
def vdirsyncer-auth [auth]: nothing -> string {
  if $auth == null { return "" }
  let password = if $auth.password? != null {
    $"password = ($auth.password | to json --raw)"
  } else {
    $"password.fetch = ([command cat ($auth.passwordFile | path expand)] | to json --raw)"
  }
  $"\nusername = ($auth.username | to json --raw)\n($password)"
}

def vdirsyncer-config [calendars: list, state: path]: nothing -> string {
  $calendars | each {|c| $"
[pair ($c.name)]
a = \"($c.name)_remote\"
b = \"($c.name)_local\"
collections = null

[storage ($c.name)_remote]
type = \"http\"
url = ($c.url | to json --raw)(vdirsyncer-auth $c.basicAuth?)

[storage ($c.name)_local]
type = \"filesystem\"
path = ($state | path join vdirs $c.name | to json --raw)
fileext = \".ics\""
  } | prepend $"[general]\nstatus_path = ($state | path join status | to json --raw)" | str join "\n"
}

def khal-config [calendars: list, state: path]: nothing -> string {
  $calendars
  | each {|c| $"[[($c.name)]]\npath = ($state | path join vdirs $c.name)\ncolor = \"($c.color)\"\nreadonly = True" }
  | prepend "[calendars]"
  | append $"[sqlite]\npath = ($state | path join khal.db)"
  | append $KHAL_LOCALE
  | str join "\n"
}

# Each occurrence once, in desktop-shell's terms. khal lists an event once per day it spans, and --once would drop a
# repeating event's later occurrences.
def events [config: path]: nothing -> list {
  let from = (date now) - 62day | format date "%F"
  let to = (date now) + 366day | format date "%F"
  let fields = [uid title start-date start-time end-date end-time all-day location calendar-color]
  ^khal -c $config list ...($fields | each {|f| [--json $f] } | flatten) $from $to
  | lines
  | each { from json }
  | flatten
  | uniq-by uid start-date start-time
  | each {|e|
    let all_day = $e.all-day == "True"
    let event = {
      title: $e.title
      start: (if $all_day { $e.start-date } else { $"($e.start-date)T($e.start-time)" })
      end: (if $all_day { $e.end-date } else { $"($e.end-date)T($e.end-time)" })
      allDay: $all_day
      color: $e.calendar-color
    }
    if ($e.location | is-empty) { $event } else { $event | insert location $e.location }
  }
}

# vdirsyncer's first error line, its addresses left out: one can be secret.
def reason [stderr: string]: nothing -> string {
  let errors = $stderr | lines | where {|line| $line =~ '(?i)error' }
  if ($errors | is-empty) { return "could not sync" }
  $errors | first | str replace --all --regex 'https?://\S+' '<address>' | str replace --regex '^.*?error:\s*' '' | str trim
}

# Replaced whole, so the shell never reads half a file.
def write-events [file: path, content: record] {
  $content | to json | save -f $"($file).tmp"
  mv -f $"($file).tmp" $file
}

def main [calendars_file: path, events_file: path] {
  # It holds the calendars' addresses and logins.
  let state = $events_file | path dirname
  mkdir $state
  ^chmod 700 $state
  let previous = if ($events_file | path exists) { try { open $events_file } catch { {} } } else { {} }
  let kept = { updated: ($previous.updated? | default (date now | format date "%Y-%m-%dT%H:%M")), events: ($previous.events? | default []) }

  let calendars = try { read-calendars $calendars_file } catch {|e|
    write-events $events_file ($kept | insert error $e.msg)
    error make --unspanned { msg: $e.msg }
  }
  if ($calendars | is-empty) {
    print $"No calendars in ($calendars_file), so no events."
    rm -f $events_file
    return
  }
  write-events $events_file ($kept | insert syncing true)

  for c in $calendars { mkdir ($state | path join vdirs $c.name) }
  let vdirsyncer = $state | path join vdirsyncer.conf
  let khal = $state | path join khal.conf
  vdirsyncer-config $calendars $state | save -f $vdirsyncer
  khal-config $calendars $state | save -f $khal

  # One calendar at a time, so a failure names it and its reason and keeps its last events without holding back the
  # others. Discover is a no-op once the pairs are known, and needed whenever one is added.
  let discovered = ^vdirsyncer -c $vdirsyncer discover | complete
  let failures = if $discovered.exit_code != 0 {
    $calendars | each {|c| { name: $c.name, reason: (reason $discovered.stderr) } }
  } else {
    $calendars | each {|c|
      let run = ^vdirsyncer -c $vdirsyncer sync $c.name | complete
      if $run.exit_code != 0 { { name: $c.name, reason: (reason $run.stderr) } }
    } | compact
  }
  let events = try { events $khal } catch { $kept.events }
  let now = date now | format date "%Y-%m-%dT%H:%M"

  if ($failures | is-empty) {
    write-events $events_file { updated: $now, events: $events }
  } else {
    let error = $failures | group-by reason | items {|reason, failed| $"($failed.name | str join ', '): ($reason)" } | str join "; "
    # Only a sync that fetched something makes the events newer.
    let updated = if ($failures | length) == ($calendars | length) { $kept.updated } else { $now }
    write-events $events_file { updated: $updated, events: $events, error: $error }
    exit 1
  }
}
