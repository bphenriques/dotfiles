use ./core.nu [fmt-currency pct]

export def chart [title: string, color: string] {
  $in | to tsv --noheaders | ^uplot bar -t $title -c $color -d "\t"
}

export def render-summary [data: record] {
  let d = ansi white_dimmed
  let r = ansi reset
  let bal_color = if $data.free >= 0 { ansi green } else { ansi red }
  print $"\n  Income        (ansi green)(fmt-currency $data.income | fill -a right -w 12)($r)"
  print $"  Expenses      (ansi red)(fmt-currency $data.expense | fill -a right -w 12)($r)   ($d)(pct $data.expense $data.income)%($r)"
  print $"  Savings       (ansi blue)(fmt-currency $data.savings | fill -a right -w 12)($r)   ($d)(pct $data.savings $data.income)%($r)"
  print $"  Transfer/mo   (fmt-currency $data.transfer | fill -a right -w 12)"
  print $"  ($d)─────────────────────────($r)"
  print $"  (ansi white_bold)Balance       ($bal_color)(fmt-currency $data.free | fill -a right -w 12)($r)"
}

export def render-expenses-table [rows: table] {
  print ""
  let cat_hdr = $"(ansi cyan)Category(ansi reset)"
  let sub_hdr = $"(ansi cyan)Expense(ansi reset)"
  let amt_hdr = $"(ansi cyan)Amount(ansi reset)"
  $rows
    | each { |r| { $cat_hdr: $r.category, $sub_hdr: $r.sub, $amt_hdr: (fmt-currency $r.amount) } }
    | table --index false
    | print
}

export def render-savings-chart [rows: table, label: string] {
  $rows
    | each { |r| { label: $"($r.label) (fmt-currency $r.value) \(($r.pct)%\)", value: $r.value } }
    | chart $"Savings — ($label)" blue
}

export def render-split-table [rows: table, my_total: float, other_total: float] {
  if ($rows | is-empty) {
    print "  No shared expenses."
    return
  }
  print ""
  let groups = $rows | get group | uniq
  let show_group = ($groups | length) > 1
  let grp_hdr = $"(ansi cyan)Group(ansi reset)"
  let desc_hdr = $"(ansi cyan)Split Expense(ansi reset)"
  let amt_hdr = $"(ansi cyan)Amount(ansi reset)"
  let share_hdr = $"(ansi cyan)Share(ansi reset)"
  let freq_hdr = $"(ansi cyan)Frequency(ansi reset)"
  let contrib_hdr = $"(ansi cyan)Contribution(ansi reset)"
  $rows
    | each { |r|
        let base = {
          $desc_hdr: $r.description,
          $amt_hdr: (fmt-currency $r.amount),
          $share_hdr: $"($r.share * 100 | math round | into int)%",
          $freq_hdr: $r.frequency,
          $contrib_hdr: (fmt-currency $r.contribution)
        }
        if $show_group { { $grp_hdr: ($r.group | str capitalize) } | merge $base } else { $base }
      }
    | table --index false
    | print
  let d = ansi white_dimmed
  let r = ansi reset
  print $"\n  ($d)My transfer/mo:($r)    (fmt-currency $my_total)"
  print $"  ($d)Other transfer/mo:($r) (fmt-currency $other_total)"
}
