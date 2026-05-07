use ./core.nu [pct amt MONTHLY_DIVISOR]
use ./budget.nu [load-budget enrich-budget]

# Monthly view: smoothed (monthly + quarterly/3 + yearly/12). Yearly view: yearly cash flow.
def normalize [row: record, eff_amount: float, is_month: bool]: nothing -> float {
  let div = $MONTHLY_DIVISOR | get $row.frequency
  if $is_month { $eff_amount / $div } else { $eff_amount * 12 / $div }
}

export def dashboard-data [period: string]: nothing -> record {
  let is_month = $period == "month"
  let label = if $is_month { "monthly €" } else { "annual €" }
  let enriched = load-budget | enrich-budget

  let normalized = $enriched | each { |e|
    let amount = (normalize $e.row $e.effective_amount $is_month) | amt
    { kind: $e.kind, category: $e.row.category, amount: $amount, split_name: $e.split_name }
  } | where amount != 0

  let income = $normalized | where kind == "income" | get amount | math sum | amt
  let expense = $normalized | where kind == "expense" | get amount | math sum | amt
  let savings = $normalized | where kind == "savings" | get amount | math sum | amt
  let personal_expense = $normalized | where kind == "expense" and split_name == null | get amount | math sum | amt
  let joint_expense = ($expense - $personal_expense) | amt

  let exp_split = $normalized | where kind == "expense" | each { |r|
    let parts = $r.category | split row '>' | each { str trim }
    { category: $parts.0, sub: ($parts | skip 1 | str join " > "), amount: $r.amount }
  }
  let expense_groups = $exp_split
    | insert _k { |r| $"($r.category)|($r.sub)" }
    | group-by --to-table _k
    | each { |g|
        let f = $g.items | first
        { category: $f.category, sub: $f.sub, amount: ($g.items | get amount | math sum | amt) }
      }
    | group-by --to-table category
    | each { |g|
        {
          category: $g.category,
          total: ($g.items | get amount | math sum | amt),
          items: ($g.items | sort-by amount --reverse | each { |r| { sub: $r.sub, amount: $r.amount } })
        }
      }
    | sort-by total --reverse

  let sav_rows = $normalized | where kind == "savings"
    | group-by --to-table category
    | each { |g|
        let v = $g.items | get amount | math sum | amt
        { label: $g.category, value: $v, pct: (pct $v $savings) }
      }
    | sort-by value --reverse

  let free = $income - $expense - $savings | amt

  {
    income: $income, expense: $expense, savings: $savings,
    personal_expense: $personal_expense, joint_expense: $joint_expense,
    free: $free, label: $label,
    expense_groups: $expense_groups, savings_rows: $sav_rows,
  }
}
