use ./core.nu [fmt-currency pct]

export def chart [title: string, color: string] {
  $in | to tsv --noheaders | ^uplot bar -t $title -c $color -d "\t"
}

export def render-summary [data: record, loc: record] {
  let fc = { |n| fmt-currency $n $loc }
  let d = ansi white_dimmed
  let r = ansi reset
  let bal_color = if $data.free >= 0 { ansi green } else { ansi red }
  print $"\n  Income        (ansi green)(do $fc $data.income | fill -a right -w 12)($r)"
  print $"  Expenses      (ansi red)(do $fc $data.expense | fill -a right -w 12)($r)   ($d)(pct $data.expense $data.income)%($r)"
  print $"  Savings       (ansi blue)(do $fc $data.savings | fill -a right -w 12)($r)   ($d)(pct $data.savings $data.income)%($r)"
  print $"  ($d)Transfer/mo($r)   (do $fc $data.transfer | fill -a right -w 12)"
  print $"  Balance       ($bal_color)(do $fc $data.free | fill -a right -w 12)($r)"
}

export def render-expenses-table [rows: table, loc: record] {
  print ""
  let cat_hdr = $"(ansi red)Category(ansi reset)"
  let sub_hdr = $"(ansi red)Expense(ansi reset)"
  let amt_hdr = $"(ansi red)Amount(ansi reset)"
  $rows
    | each { |r| { $cat_hdr: $r.category, $sub_hdr: $r.sub, $amt_hdr: (fmt-currency $r.amount $loc) } }
    | table --index false
    | print
}

export def render-savings-chart [rows: table, label: string] {
  $rows
    | each { |r| { label: $"($r.label) \(($r.pct)%\)", value: $r.value } }
    | chart $"Savings — ($label)" blue
}

export def render-split-table [rows: table, my_total: float, other_total: float, loc: record, period: string = "month"] {
  if ($rows | is-empty) {
    print "  No shared expenses."
    return
  }
  print ""
  if $period == "year" {
    print $"  (ansi white_dimmed)(ansi white_italic)monthly breakdown — split expenses are not annualized(ansi reset)"
  }
  let groups = $rows | get group | uniq
  let show_group = ($groups | length) > 1
  let grp_hdr = $"(ansi red)Group(ansi reset)"
  let desc_hdr = $"(ansi red)Split Expense(ansi reset)"
  let amt_hdr = $"(ansi red)Amount(ansi reset)"
  let share_hdr = $"(ansi red)Share(ansi reset)"
  let contrib_hdr = $"(ansi red)Monthly Contribution(ansi reset)"
  $rows
    | each { |r|
        let base = {
          $desc_hdr: $r.description,
          $amt_hdr: (fmt-currency $r.amount $loc),
          $share_hdr: $"($r.share * 100 | math round | into int)%",
          $contrib_hdr: (fmt-currency $r.contribution $loc)
        }
        if $show_group { { $grp_hdr: ($r.group | str capitalize) } | merge $base } else { $base }
      }
    | table --index false
    | print
  let d = ansi white_dimmed
  let r = ansi reset
  print $"\n  ($d)My transfer/mo:($r)    (fmt-currency $my_total $loc)"
  print $"  ($d)Other transfer/mo:($r) (fmt-currency $other_total $loc)"
}
