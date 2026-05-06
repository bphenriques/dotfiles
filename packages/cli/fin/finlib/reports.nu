use ./core.nu [prettify-slug pct amt KIND_PREFIX MONTHLY_DIVISOR]
use ./ledger.nu [run-hledger]
use ./budget.nu [split-monthly-transfer split-breakdown]

def hledger-amounts [args: list<string>]: nothing -> table<account: string, amount: float> {
  let json = (run-hledger ($args | append ["-O" "json"])) | from json
  let rows = $json | get 0
  $rows | each { |r|
    let account = $r | get 0
    let amounts = $r | get 3
    let amount = if ($amounts | is-empty) { 0.0 } else { $amounts | get 0.aquantity.floatingPoint | into float }
    { account: $account, amount: $amount }
  }
}

def leaf-accounts []: table -> table {
  let data = $in
  $data | where { |r| ($data | where { |o| $o.account | str starts-with $"($r.account):" } | is-empty) }
}

export def dashboard-data [period: string]: nothing -> record {
  let is_month = $period == "month"
  let forecast = if $is_month { "--forecast=this month" } else { "--forecast=this year" }
  let time = if $is_month { "this month" } else { "this year" }
  let label = if $is_month { "monthly €" } else { "annual €" }

  # Summary totals
  let income = (hledger-amounts [balance -E --depth 1 $forecast -p $time --invert $KIND_PREFIX.income])
    | where account == $KIND_PREFIX.income | get amount | append 0.0 | first | amt
  let expense = (hledger-amounts [balance -E --depth 1 $forecast -p $time $KIND_PREFIX.expense])
    | where account == $KIND_PREFIX.expense | get amount | append 0.0 | first | amt
  let savings = (hledger-amounts [balance -E --depth 2 $forecast -p $time $KIND_PREFIX.savings])
    | where account == $KIND_PREFIX.savings | get amount | append 0.0 | first | amt

  let transfer = split-monthly-transfer | get total | append 0 | math sum

  # Expense breakdown
  let exp_rows = (hledger-amounts [balance -E --depth 3 $forecast -p $time $KIND_PREFIX.expense])
    | leaf-accounts
    | each { |r|
      let parts = ($r.account | str replace $"($KIND_PREFIX.expense):" "" | split row ":")
      { category: ($parts | first | prettify-slug), sub: (if ($parts | length) > 1 { $parts | skip 1 | each { prettify-slug } | str join " " } else { "" }), amount: ($r.amount | amt) }
    }
  let cat_totals = $exp_rows | group-by category | transpose key value | each { |g| { category: $g.key, total: ($g.value | get amount | math sum) } }
  let exp_rows = $exp_rows | each { |r|
    let cat_total = $cat_totals | where category == $r.category | first | get total
    $r | insert _cat_total $cat_total
  } | sort-by _cat_total amount --reverse | reject _cat_total

  # Savings breakdown
  let sav_rows = (hledger-amounts [balance -E --depth 3 $forecast -p $time $KIND_PREFIX.savings])
    | leaf-accounts
    | each { |r|
      { label: ($r.account | str replace $"($KIND_PREFIX.savings):" "" | prettify-slug), value: ($r.amount | amt), pct: (pct $r.amount $savings) }
    } | sort-by value --reverse

  # Split breakdown
  let split_rows = split-breakdown
  let periodic = $split_rows | where { |r| $r.frequency in [monthly quarterly yearly] }
  let my_total = $periodic | each { |r| $r.contribution / ($MONTHLY_DIVISOR | get $r.frequency) } | append 0 | math sum | amt
  let other_total = $periodic | each { |r| ($r.amount - $r.contribution) / ($MONTHLY_DIVISOR | get $r.frequency) } | append 0 | math sum | amt

  let free = $income - $expense - $savings | amt

  {
    income: $income, expense: $expense, savings: $savings, transfer: $transfer, free: $free, label: $label,
    expenses: $exp_rows, savings_rows: $sav_rows,
    split_rows: $split_rows, split_my_total: $my_total, split_other_total: $other_total
  }
}
