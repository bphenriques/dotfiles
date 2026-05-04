export const MONTH_RE = '^\d{4}-(0[1-9]|1[0-2])$'

export def fin-dir []: nothing -> string {
  let d = ($env.FIN_DIR? | default "")
  if $d == "" or not ($d | path exists) {
    print "fin: FIN_DIR not set or not accessible" --stderr
    exit 2
  }
  $d
}

export def runtime-dir []: nothing -> string {
  let d = ($env.XDG_RUNTIME_DIR? | default "")
  if $d == "" {
    error make { msg: "fin: XDG_RUNTIME_DIR is not set" }
  }
  $d
}

export def fmt-eur [n: float]: nothing -> string {
  let sign = if $n < 0 { "-" } else { "" }
  let total_cents = (($n | math abs) * 100 | math round | into int)
  let whole = $total_cents // 100
  let frac = ($total_cents mod 100 | into string | fill -a right -w 2 -c '0')
  let whole_str = if $whole >= 1_000_000 {
    let m = $whole // 1_000_000
    let k = ($whole mod 1_000_000 // 1000 | into string | fill -a right -w 3 -c '0')
    let u = ($whole mod 1000 | into string | fill -a right -w 3 -c '0')
    $"($m).($k).($u)"
  } else if $whole >= 1000 {
    let k = $whole // 1000
    let u = ($whole mod 1000 | into string | fill -a right -w 3 -c '0')
    $"($k).($u)"
  } else {
    $whole | into string
  }
  $"($sign)€($whole_str),($frac)"
}

export def pct [part: float, total: float]: nothing -> float {
  if $total == 0 { 0.0 } else { ($part / $total * 100 | math round --precision 1) }
}

export def prettify-slug []: string -> string {
  split row "-" | each { str capitalize } | str join " "
}

export def parse-balance []: string -> float {
  str replace --all '.' '' | str replace ',' '.' | into float
}
