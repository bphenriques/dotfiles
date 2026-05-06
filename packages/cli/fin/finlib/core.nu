export const KIND_PREFIX = { income: "revenues", expense: "expenses", savings: "assets:savings" }
export const PRIMARY_BANK = "assets:banco:conta-pessoal"

export def fin-dir []: nothing -> string {
  let d = ($env.FIN_DIR? | default "")
  if $d != "" {
    if not ($d | path exists) { error make { msg: $"fin: FIN_DIR '($d)' not accessible" } }
    return $d
  }
  let md = ($env.FIN_MARKDOWN? | default "")
  if $md == "" { error make { msg: "fin: set FIN_DIR or FIN_MARKDOWN" } }
  let fallback = [(runtime-dir) fin data] | path join
  mkdir $fallback
  $fallback
}

export def runtime-dir []: nothing -> string {
  let d = ($env.XDG_RUNTIME_DIR? | default "")
  if $d == "" {
    error make { msg: "fin: XDG_RUNTIME_DIR is not set" }
  }
  $d
}

export def md-source []: nothing -> string {
  $env.FIN_MARKDOWN? | default ""
}

export def slug-segment []: string -> string {
  str downcase | str trim
    | str replace --all 'á' 'a' | str replace --all 'à' 'a' | str replace --all 'ã' 'a' | str replace --all 'â' 'a'
    | str replace --all 'é' 'e' | str replace --all 'ê' 'e'
    | str replace --all 'í' 'i'
    | str replace --all 'ó' 'o' | str replace --all 'ô' 'o' | str replace --all 'õ' 'o'
    | str replace --all 'ú' 'u'
    | str replace --all 'ç' 'c'
    | str replace --regex '[^a-z0-9]+' '-'
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

# Read all monetary locale keys in one subprocess call. Call once and pass the result around.
export def monetary-locale []: nothing -> record {
  let raw = if ($env.FIN_LOCALE? | default "" | is-not-empty) {
    (do { ^env $"LC_ALL=($env.FIN_LOCALE)" locale -k LC_MONETARY } | complete).stdout
  } else {
    (do { ^locale -k LC_MONETARY } | complete).stdout
  }
  let lines = $raw | lines
  let get = { |key: string, fallback: string|
    let match = $lines | where { |l| $l | str starts-with $"($key)=" } | first
    if ($match | is-empty) { $fallback } else {
      $match | str replace $"($key)=" "" | str replace --all '"' ''
    }
  }
  {
    symbol: (do $get "currency_symbol" "€")
    decimal: (do $get "mon_decimal_point" ",")
    separator: (do $get "mon_thousands_sep" ".")
    grouping: (try { do $get "mon_grouping" "3" | into int } catch { 3 })
    p_cs_precedes: (try { do $get "p_cs_precedes" "1" | into int } catch { 1 })
    p_sep_by_space: (try { do $get "p_sep_by_space" "0" | into int } catch { 0 })
    n_cs_precedes: (try { do $get "n_cs_precedes" "1" | into int } catch { 1 })
    n_sep_by_space: (try { do $get "n_sep_by_space" "0" | into int } catch { 0 })
  }
}

def group-digits [digits: string, separator: string, grouping: int]: nothing -> string {
  if $grouping <= 0 or $separator == "" { return $digits }
  let chars = ($digits | split chars)
  let len = ($chars | length)
  mut result = ""
  for i in 0..<$len {
    let pos = $len - $i
    if $i > 0 and ($pos mod $grouping) == 0 {
      $result = $"($result)($separator)"
    }
    $result = $"($result)($chars | get $i)"
  }
  $result
}

def fmt-with-locale [n: float, loc: record]: nothing -> string {
  let v = $n | amt
  let is_neg = $v < 0
  let total_cents = (($v | math abs) * 100 | math round | into int)
  let whole = $total_cents // 100
  let frac = ($total_cents mod 100 | into string | fill -a right -w 2 -c '0')
  let digits = group-digits ($whole | into string) $loc.separator $loc.grouping
  let number = $"($digits)($loc.decimal)($frac)"

  let cs_precedes = if $is_neg { $loc.n_cs_precedes } else { $loc.p_cs_precedes }
  let sep_by_space = if $is_neg { $loc.n_sep_by_space } else { $loc.p_sep_by_space }
  let space = if $sep_by_space == 1 { " " } else { "" }
  let sign = if $is_neg { "-" } else { "" }

  let formatted = if $cs_precedes == 1 {
    $"($loc.symbol)($space)($number)"
  } else {
    $"($number)($space)($loc.symbol)"
  }
  $"($sign)($formatted)"
}

# Format amount for display using a pre-loaded locale record.
export def fmt-currency [n: float, loc: record]: nothing -> string {
  fmt-with-locale $n $loc
}

export def pct [part: float, total: float]: nothing -> float {
  if $total == 0 { 0.0 } else { ($part / $total * 100 | math round --precision 1) }
}

export def prettify-slug []: string -> string {
  split row "-" | each { str capitalize } | str join " "
}
