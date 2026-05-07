export const MONTHLY_DIVISOR = { monthly: 1, quarterly: 3, yearly: 12 }

export def fin-dir []: nothing -> string {
  $env.FIN_DIR? | default ""
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
