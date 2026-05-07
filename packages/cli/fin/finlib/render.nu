use ./core.nu [fmt-currency pct]

const ICON_INCOME = "↑"
const ICON_EXPENSE = "↓"
const ICON_SAVINGS = "●"
const ICON_WALLET = "○"
const ICON_EXPANDED = "▼"
const ICON_COLLAPSED = "▶"

export def render-summary [data: record] {
  let d = ansi white_dimmed
  let r = ansi reset
  let bal_color = if $data.free >= 0 { ansi green_bold } else { ansi red_bold }
  print $"\n  ($ICON_INCOME) Income        (ansi green_bold)(fmt-currency $data.income | fill -a right -w 12)($r)"
  print $"  ($ICON_EXPENSE) Expenses      (ansi red_bold)(fmt-currency $data.expense | fill -a right -w 12)($r)   ($d)(pct $data.expense $data.income)%($r)"
  print $"    ($d)Personal($r)    (fmt-currency $data.personal_expense | fill -a right -w 12)   ($d)(pct $data.personal_expense $data.income)%($r)"
  print $"    ($d)Joint($r)       (ansi magenta)(fmt-currency $data.joint_expense | fill -a right -w 12)($r)   ($d)(pct $data.joint_expense $data.income)%($r)"
  print $"  ($ICON_SAVINGS) Savings       (ansi blue_bold)(fmt-currency $data.savings | fill -a right -w 12)($r)   ($d)(pct $data.savings $data.income)%($r)"
  print $"  ($d)─────────────────────────($r)"
  print $"  ($ICON_WALLET) (ansi white_bold)Unallocated   ($bal_color)(fmt-currency $data.free | fill -a right -w 12)($r)"
}

export def render-expenses-table [groups: table, income: float, detailed: bool] {
  let d = ansi white_dimmed
  let r = ansi reset
  let icon = if $detailed { $ICON_EXPANDED } else { $ICON_COLLAPSED }
  print ""
  for g in $groups {
    let g_pct = pct $g.total $income
    print $"  (ansi red)($icon)($r) (ansi red_bold)($g.category)($r) — (ansi red_bold)(fmt-currency $g.total)($r) ($d)\(($g_pct)%\)($r)"
    if $detailed {
      let single_no_sub = ($g.items | length) == 1 and ($g.items.0.sub == "")
      if not $single_no_sub {
        for item in $g.items {
          let label = if $item.sub == "" { "—" } else { $item.sub }
          let row_pct = pct $item.amount $income
          print $"      ($label | fill -a left -w 28) (fmt-currency $item.amount | fill -a right -w 12)   ($d)($row_pct)%($r)"
        }
      }
    }
  }
}

# Total appears in the summary section; only render the per-category breakdown on --detailed.
export def render-savings-table [rows: table, income: float, detailed: bool] {
  if not $detailed { return }
  let d = ansi white_dimmed
  let r = ansi reset
  print ""
  for row in $rows {
    let row_pct = pct $row.value $income
    print $"  (ansi blue)($ICON_EXPANDED)($r) (ansi blue_bold)($row.label)($r) — (ansi blue_bold)(fmt-currency $row.value)($r) ($d)\(($row_pct)%\)($r)"
  }
}
