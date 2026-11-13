# Alfredo from the laptop, through his guest over SSH, so the API key never leaves the guest.

const instructions = "This reply is shown in a small plain-text panel. Write plain text only: no Markdown, no tables, no bold, no headings. Put list items on short separate lines."

# The guest's own API on its loopback, keyed with the copy it already holds.
def api [path: string] {
  ^ssh $env.SSH_HOST $"curl -sSN -H \"Authorization: Bearer $\(sudo cat ($env.API_KEY_FILE))\" -H 'Content-Type: application/json' -d @- http://127.0.0.1:($env.API_PORT)($path)"
}

def fail [message: string = "Could not reach Alfredo"] {
  print ({error: $message} | to json --raw)
  exit 0
}

# Ids go into a command the guest's shell reads, so only the characters Hermes uses for them pass.
def checked [] {
  if $in !~ '^[A-Za-z0-9_-]+$' { error make {msg: $"Not an id: ($in)"} }
  $in
}

const column_width = 30

# Words in lines of at most `width` characters; a longer word keeps its own line.
def wrap [width: int] {
  split row " " | reduce --fold [] {|word, lines|
    let current = $lines | last
    if $current == null { [$word] } else if (($current | str length --grapheme-clusters) + 1 + ($word | str length --grapheme-clusters)) <= $width {
      $lines | drop | append $"($current) ($word)"
    } else { $lines | append $word }
  }
}

# Markdown tables as aligned columns, long cells wrapped inside theirs. Tools write tables (the meals plan), and the model
# relays them as written, whatever it is asked.
def untable [] {
  let separator = '^\|?\s*:?-+:?\s*(\|\s*:?-+:?\s*)+\|?$'
  # Runs of lines with a pipe; a run is a table when it has a separator row (outer pipes are optional in Markdown).
  let blocks = $in | lines | reduce --fold [] {|line, blocks|
    let row = $line | str contains "|"
    let last = $blocks | last
    if $row and $last != null and $last.row { $blocks | drop | append {row: true, lines: ($last.lines | append $line)} } else {
      $blocks | append {row: $row, lines: [$line]}
    }
  }
  $blocks | each {|block|
    if not ($block.row and ($block.lines | any {|line| ($line | str trim) =~ $separator })) { return $block.lines }
    let rows = $block.lines
      | where {|line| ($line | str trim) !~ $separator }
      | each {|line| $line | str trim | str trim --char "|" | split row "|" | each { str trim | wrap $column_width } }
    let columns = $rows | each { length } | math max
    let widths = 0..<$columns | each {|i| $rows | each {|row| $row | get --optional $i | default [] | each { str length --grapheme-clusters } | append 0 | math max } | math max }
    $rows | each {|row|
      let height = $row | each { length } | math max
      0..<$height | each {|n|
        0..<$columns | each {|i|
          let text = $row | get --optional $i | default [] | get --optional $n | default ""
          if $i == $columns - 1 { $text } else { $text | fill --width ($widths | get $i) }
        } | str join "  " | str trim --right
      }
    } | flatten
  } | flatten | str join "\n"
}

# The reply as the shell shows it: plain text.
def plain [] {
  untable | str replace --all --regex '\*\*|__|`' '' | str replace --all --regex '(?m)^#+ ' ''
}

# One server-sent event line, read with the line before it (its `event:` name), as what `ask` prints, or nothing.
def frame [previous: string, line: string, session: string] {
  # Not a stream at all: the API refused the request.
  if ($line | str starts-with "{") {
    return {error: ($line | from json | get --optional error.message | default "No answer")}
  }
  if not ($line | str starts-with "data: ") { return null }
  let data = $line | str substring 6.. | from json
  match ($previous | str replace "event: " "") {
    "run.started" => {run: $data.run_id, session: $session}
    # Waits for `approve`, or fails closed after approvals.timeout.
    "approval.request" => {approval: ($data.command? | default $data.description? | default "an action")}
    # An MCP tool by its server (mcp__meals__plan_week: meals), the rest by name.
    "tool.started" => {status: (if ($data.tool_name | str starts-with "mcp__") { $data.tool_name | split row "__" | get 1 } else { $data.tool_name })}
    "assistant.completed" => (if ($data.interrupted? | default false) { {error: "Stopped"} } else { {session: $session, reply: ($data.content | plain)} })
    "error" => {error: $data.message}
    _ => null
  }
}

# Opens the session, or a new one, in Hermes' terminal interface.
def "main open" [session: string = ""] {
  let resume = if $session == "" { [] } else { [--resume ($session | checked)] }
  exec ssh -t $env.SSH_HOST sudo -u hermes hermes --tui ...$resume
}

# Answers a run's pending approval: allow (once) or deny.
def "main approve" [run: string, answer: string] {
  {choice: $answer} | to json --raw | api $"/v1/runs/($run | checked)/approval" | ignore
}

# Stops a run on the guest, not only its stream here.
def "main stop" [run: string] {
  '{}' | api $"/v1/runs/($run | checked)/stop" | ignore
}

# The message: the prompt, and an image with it, shrunk to fit the request (Hermes takes 10 MB) and sent inline.
def message [prompt: string, image: string] {
  if $image == "" { return $prompt }
  let data = try { ^magick $image -resize "1600x1600>" png:- | encode base64 } catch { fail "Could not read the image" }
  [{type: text, text: $prompt}, {type: image_url, image_url: {url: $"data:image/png;base64,($data)"}}]
    | where {|part| $part.type != "text" or $part.text != "" }
}

# One turn, with an image when given. Prints a JSON object per line: {"run", "session"} first, {"status"} for each tool it
# uses, {"approval"} when it waits for one, then {"session", "reply"} or {"error"}.
def "main ask" [prompt: string, session: string = "", image: string = ""] {
  let session = if $session != "" { try { $session | checked } catch { fail "Not a session" } } else {
    let created = try { '{}' | api /api/sessions | from json } catch { fail }
    $created.session?.id? | default { fail ($created.error?.message? | default "Could not start a session") } | checked
  }
  # For this turn only, so the session keeps its Markdown when it moves to the terminal.
  let printed = {message: (message $prompt $image), instructions: $instructions} | to json --raw
    | api $"/api/sessions/($session)/chat/stream"
    | lines
    | prepend ""
    | window 2
    | each {|pair| frame $pair.0 $pair.1 $session }
    | compact
    | each {|answer| print ($answer | to json --raw); $answer }
  if not ($printed | any {|answer| $answer.reply? != null or $answer.error? != null }) { fail }
}

def main [] {
  main open
}
