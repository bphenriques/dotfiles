# Parse hledger EUR amount (€1.234,56 → 1234.56)
def parse-eur []: string -> float {
  str replace --all '€' '' | str replace --all '.' '' | str replace ',' '.' | into float
}

def fmt-eur [n: float]: nothing -> string {
  let sign = if $n < 0 { "-" } else { "" }
  let total_cents = (($n | math abs) * 100 | math round | into int)
  $"($sign)€(($total_cents // 100)),($total_cents mod 100 | into string | fill -a right -w 2 -c '0')"
}

def pct [part: float, total: float]: nothing -> float {
  if $total == 0 { 0.0 } else { ($part / $total * 100 | math round --precision 1) }
}

def chart [title: string, color: string] {
  $in | to tsv --noheaders | ^uplot bar -t $title -c $color -d "\t"
}

def main [] {
  let month_label = date now | format date "%B %Y"

  # ── Gather data ──
  let summary = (hledger balance -E --depth 1 --forecast='this year' -p 'this month' -O csv revenues expenses assets:savings
    | from csv | where account != "Total:" | update balance { parse-eur })

  let income = ($summary | where account == "revenues" | get balance.0 | math abs)
  let expenses = ($summary | where account == "expenses" | get balance.0)
  let savings = ($summary | where account == "assets" | get balance.0)

  let shared_total = (hledger balance -p 'this month' --forecast='this month' desc:partilhados not:assets:banco -O csv --depth 0
    | from csv | where account == "Total:" | get balance.0 | parse-eur)

  # ── Summary ──
  print $"\n  (ansi green)Income(ansi reset)        (fmt-eur $income | fill -a right -w 12)"
  print $"  (ansi red)Expenses(ansi reset)      (fmt-eur $expenses | fill -a right -w 12)   (pct $expenses $income)%"
  print $"  (ansi blue)Savings(ansi reset)       (fmt-eur $savings | fill -a right -w 12)   (pct $savings $income)%"
  print $"  Transfer/mo   (fmt-eur $shared_total | fill -a right -w 12)"
  print $"  Free          (fmt-eur ($income - $expenses - $savings) | fill -a right -w 12)"

  # ── Charts ──
  hledger balance -E --depth 2 --forecast='this year' -p 'this month' -O csv expenses
    | from csv | where account != "Total:" | update balance { parse-eur }
    | each { |r| { label: ($r.account | str replace 'expenses:' ''), value: $r.balance } }
    | sort-by value --reverse
    | chart $"Expenses — ($month_label)" red

  hledger balance -E --depth 3 --forecast='this year' -p 'this month' -O csv assets:savings
    | from csv | where account != "Total:" | update balance { parse-eur }
    | each { |r| { label: ($r.account | str replace 'assets:savings:' ''), value: $r.balance } }
    | sort-by value --reverse
    | chart $"Savings — ($month_label)" blue

  [[label value]; [Income $income] [Expenses $expenses] [Savings $savings]]
    | chart "Annual Split (monthly)" cyan
}
