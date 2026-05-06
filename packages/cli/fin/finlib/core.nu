export const KIND_PREFIX = { income: "revenues", expense: "expenses", savings: "assets:savings" }
export const PRIMARY_BANK = "assets:banco:conta-pessoal"
export const FREQUENCY_RE = '^\d{4}-(0[1-9]|1[0-2])$'
export const MONTHLY_DIVISOR = { monthly: 1, quarterly: 3, yearly: 12 }

export def runtime-dir []: nothing -> string {
  $env.XDG_RUNTIME_DIR? | default ($env.TMPDIR? | default "/tmp")
}

export def fin-dir []: nothing -> string {
  $env.FIN_DIR? | default ""
}

export def health-path []: nothing -> string {
  let p = ($env.FIN_HEALTH? | default "")
  if $p != "" { return $p }
  ([(runtime-dir) fin "fin-health.md"] | path join)
}

export def slug-segment []: string -> string {
  str downcase | str trim
    | str replace --all 'á' 'a' | str replace --all 'à' 'a' | str replace --all 'ã' 'a' | str replace --all 'â' 'a'
    | str replace --all 'é' 'e' | str replace --all 'ê' 'e'
    | str replace --all 'í' 'i'
    | str replace --all 'ó' 'o' | str replace --all 'ô' 'o' | str replace --all 'õ' 'o'
    | str replace --all 'ú' 'u'
    | str replace --all 'ç' 'c'
    | str replace --all --regex '[^a-z0-9]+' '-'
    | str trim --char '-'
}

export def category-to-account [category: string, kind: string]: nothing -> string {
  let prefix = ($KIND_PREFIX | get $kind)
  let segments = $category | split row '>' | each { str trim | slug-segment } | where { |s| $s != "" }
  [$prefix] | append $segments | str join ":"
}

export def amt []: number -> float {
  into float | math round --precision 2
}

# pt_PT format: "1.234,56 €". Symbol via FIN_CURRENCY (default €).
export def fmt-currency [n: float]: nothing -> string {
  let v = $n | amt
  let sign = if $v < 0 { "-" } else { "" }
  let total_cents = (($v | math abs) * 100 | math round | into int)
  let whole = $total_cents // 100
  let frac = ($total_cents mod 100 | into string | fill -a right -w 2 -c '0')

  let chars = ($whole | into string | split chars)
  let len = ($chars | length)
  mut digits = ""
  for i in 0..<$len {
    let pos = $len - $i
    if $i > 0 and ($pos mod 3) == 0 { $digits = $"($digits)." }
    $digits = $"($digits)($chars | get $i)"
  }
  let symbol = ($env.FIN_CURRENCY? | default "€")
  $"($sign)($digits),($frac) ($symbol)"
}

export def pct [part: float, total: float]: nothing -> float {
  if $total == 0 { 0.0 } else { ($part / $total * 100 | math round --precision 1) }
}

export def prettify-slug []: string -> string {
  split row "-" | each { str capitalize } | str join " "
}
