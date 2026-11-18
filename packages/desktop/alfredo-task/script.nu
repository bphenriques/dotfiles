# Alfredo adds it with his own tool and a sync brings it here; if he does not answer, or adds nothing, it goes in as written.
def main [text: string] {
  let before = titles
  let answer = try { ^timeout 30 $env.ALFREDO ask $"New reminder: ($text)" | lines | each {|line| $line | from json } } catch { [] }
  if ($answer | any {|line| $line.reply? != null }) {
    ^sh -c $env.SYNC
    if (titles | any {|title| $title not-in $before }) { return }
  }
  ^sh -c $"($env.FALLBACK) \"$1\"" sh $text
}

def titles []: nothing -> list {
  try { open $env.TASKS_FILE | get tasks.title } catch { [] }
}
