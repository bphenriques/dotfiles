use ./core.nu [parse-balance prettify-slug pct]
use ./ledger.nu [run-hledger shared-transfer]

def leaf-accounts []: table -> table {
  let data = $in
  $data | where { |r| ($data | where { |o| $o.account | str starts-with $"($r.account):" } | is-empty) }
}

export def dashboard-data [period: string]: nothing -> record {
  let is_month = $period == "month"
  let forecast = if $is_month { "--forecast=this month" } else { "--forecast=this year" }
  let time = if $is_month { "this month" } else { "this year" }
  let label = if $is_month { "monthly €" } else { "annual €" }

  # Summary totals — let hledger aggregate at depth 1 (no leaf computation)
  let totals = (run-hledger [balance -E --depth 1 $forecast -p $time -O csv --layout=bare revenues expenses])
    | from csv | where account != "Total:"
  let income = ($totals | where account == "revenues" | get balance | append "0" | first | parse-balance | math abs | math round --precision 2)
  let expense = ($totals | where account == "expenses" | get balance | append "0" | first | parse-balance | math abs | math round --precision 2)

  let savings = (run-hledger [balance -E --depth 2 $forecast -p $time -O csv --layout=bare "assets:savings"])
    | from csv | where account == "assets:savings" | get balance | append "0" | first | parse-balance | math abs | math round --precision 2

  let transfer = shared-transfer

  # Expense breakdown
  let exp_rows = (run-hledger [balance -E --depth 3 $forecast -p $time -O csv --layout=bare expenses])
    | from csv | where account != "Total:" and account != "expenses" | leaf-accounts
    | each { |r|
      let v = ($r.balance | parse-balance | math round --precision 2)
      let parts = ($r.account | str replace "expenses:" "" | split row ":")
      { category: ($parts | first | prettify-slug), sub: (if ($parts | length) > 1 { $parts | skip 1 | each { prettify-slug } | str join " " } else { "" }), amount: $v }
    } | sort-by amount --reverse

  # Savings breakdown
  let sav_rows = (run-hledger [balance -E --depth 3 $forecast -p $time -O csv --layout=bare "assets:savings"])
    | from csv | where account != "Total:" and account != "assets:savings" | leaf-accounts
    | each { |r|
      let v = ($r.balance | parse-balance | math round --precision 2)
      { label: ($r.account | str replace "assets:savings:" "" | prettify-slug), value: $v, pct: (pct $v $savings) }
    } | sort-by value --reverse

  { income: $income, expense: $expense, savings: $savings, transfer: $transfer, free: ($income - $expense - $savings | math round --precision 2), label: $label, expenses: $exp_rows, savings_rows: $sav_rows }
}
