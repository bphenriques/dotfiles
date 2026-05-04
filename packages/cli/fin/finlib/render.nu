use ./core.nu [fmt-eur pct]

export def chart [title: string, color: string] {
  $in | to tsv --noheaders | ^uplot bar -t $title -c $color -d "\t"
}

export def render-summary [data: record] {
  print $"\n  (ansi green)Income(ansi reset)        (fmt-eur $data.income | fill -a right -w 12)"
  print $"  (ansi red)Expenses(ansi reset)      (fmt-eur $data.expense | fill -a right -w 12)   (pct $data.expense $data.income)%"
  print $"  (ansi blue)Savings(ansi reset)       (fmt-eur $data.savings | fill -a right -w 12)   (pct $data.savings $data.income)%"
  print $"  Transfer/mo   (fmt-eur $data.transfer | fill -a right -w 12)"
  print $"  Free          (fmt-eur $data.free | fill -a right -w 12)"
}

export def render-expenses-table [rows: table] {
  print ""
  let cat_hdr = $"(ansi red)Category(ansi reset)"
  let sub_hdr = $"(ansi red)Description(ansi reset)"
  let amt_hdr = $"(ansi red)Amount(ansi reset)"
  $rows
    | each { |r| { $cat_hdr: $r.category, $sub_hdr: $r.sub, $amt_hdr: (fmt-eur $r.amount) } }
    | table --index false
    | print
}

export def render-savings-chart [rows: table, label: string] {
  $rows
    | each { |r| { label: $"($r.label) \(($r.pct)%\)", value: $r.value } }
    | chart $"Savings — ($label)" blue
}
