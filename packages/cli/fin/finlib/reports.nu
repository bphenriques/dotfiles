use ./core.nu [prettify-slug pct amt KIND_PREFIX]
use ./ledger.nu [run-hledger shared-transfer shared-breakdown]

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

  let transfer = shared-transfer

  # Expense breakdown
  let exp_rows = (hledger-amounts [balance -E --depth 3 $forecast -p $time $KIND_PREFIX.expense])
    | leaf-accounts
    | each { |r|
      let parts = ($r.account | str replace $"($KIND_PREFIX.expense):" "" | split row ":")
      { category: ($parts | first | prettify-slug), sub: (if ($parts | length) > 1 { $parts | skip 1 | each { prettify-slug } | str join " " } else { "" }), amount: ($r.amount | amt) }
    } | sort-by amount --reverse

  # Savings breakdown
  let sav_rows = (hledger-amounts [balance -E --depth 3 $forecast -p $time $KIND_PREFIX.savings])
    | leaf-accounts
    | each { |r|
      { label: ($r.account | str replace $"($KIND_PREFIX.savings):" "" | prettify-slug), value: ($r.amount | amt), pct: (pct $r.amount $savings) }
    } | sort-by value --reverse

  # Split breakdown
  let split_rows = shared-breakdown
  let my_total = $split_rows | get contribution | append 0 | math sum | amt
  let full_total = $split_rows | get amount | append 0 | math sum | amt
  let other_total = $full_total - $my_total | amt

  let free = $income - $expense - $savings | amt

  {
    income: $income, expense: $expense, savings: $savings, transfer: $transfer, free: $free, label: $label,
    expenses: $exp_rows, savings_rows: $sav_rows,
    split_rows: $split_rows, split_my_total: $my_total, split_other_total: $other_total
  }
}
