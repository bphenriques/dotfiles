use ./core.nu [fin-dir amt MONTHLY_DIVISOR]
use ./markdown.nu [extract-budget]

const VALID_CADENCES = [monthly quarterly yearly]
const VALID_KINDS = [income expense savings]
const SPLIT_PREFIX = "split-"

export def load-budget []: nothing -> table {
  let raw = extract-budget (fin-dir)
  let splits = $raw | columns | where { |c| $c | str starts-with $SPLIT_PREFIX }
  $raw | each { |r|
    let amount = try { $r.amount | into float } catch { error make { msg: $"budget: invalid amount '($r.amount)' in category '($r.category)'" } }
    let base = $r | upsert amount $amount
    $splits | reduce -f $base { |col, acc|
      let v = ($acc | get $col | str trim)
      let parsed = if $v == "" { 0.0 } else { try { $v | into float } catch { error make { msg: $"budget: invalid split value '($v)' in category '($r.category)'" } } }
      $acc | upsert $col $parsed
    }
  }
}

# First column with weight > 0 wins (validate-budget enforces at most one).
def row-split [row: record, splits: list<string>]: nothing -> record {
  for col in $splits {
    let val = ($row | get $col)
    if $val > 0 {
      return { name: ($col | str replace $SPLIT_PREFIX ""), weight: $val }
    }
  }
  { name: null, weight: 1.0 }
}

export def enrich-budget []: table -> table {
  let rows = $in
  let splits = if ($rows | is-empty) { [] } else { $rows | first | columns | where { |c| $c | str starts-with $SPLIT_PREFIX } }
  $rows | each { |row|
    let split = row-split $row $splits
    {
      row: $row,
      kind: $row.kind,
      split_name: $split.name,
      effective_amount: ($row.amount * $split.weight | amt)
    }
  }
}

# Input: enriched budget. Returns one row per split with monthly contribution (yearly/12, quarterly/3).
export def split-monthly-transfer []: table -> table {
  $in
    | where split_name != null
    | each { |e|
        let div = $MONTHLY_DIVISOR | get $e.row.frequency
        { split_name: $e.split_name, amount: ($e.effective_amount / $div | amt) }
      }
    | group-by --to-table split_name
    | each { |g| { split_name: $g.split_name, total: ($g.items | get amount | math sum | amt) } }
    | where total > 0
}

export def validate-budget []: nothing -> list<string> {
  let raw = extract-budget (fin-dir)
  let required = [category description amount frequency kind]
  let cols = $raw | columns
  let missing = $required | where { |c| $c not-in $cols }
  if ($missing | is-not-empty) {
    return [$"budget: missing required columns: ($missing | str join ', ')"]
  }

  let splits = $cols | where { |c| $c | str starts-with $SPLIT_PREFIX }
  let budget = try { load-budget } catch { |e| return [$e.msg] }
  mut errors: list<string> = []
  for r in $budget {
    let loc = $"'($r.category)' / '($r.description)'"
    if $r.kind not-in $VALID_KINDS {
      $errors = ($errors | append $"($loc): invalid kind '($r.kind)'")
    }
    if $r.frequency not-in $VALID_CADENCES {
      $errors = ($errors | append $"($loc): invalid frequency '($r.frequency)' \(expected monthly|quarterly|yearly\)")
    }
    if ($r.category | str trim | is-empty) {
      $errors = ($errors | append "missing category")
    }
    if ($r.description | str trim | is-empty) {
      $errors = ($errors | append $"'($r.category)': missing description")
    }
    if $r.amount == 0 {
      $errors = ($errors | append $"($loc): amount must be non-zero")
    }
    for col in $splits {
      let val = ($r | get $col)
      if $val < 0 or $val > 1 {
        $errors = ($errors | append $"($loc): ($col) '($val)' must be between 0 and 1")
      }
    }
    let active_splits = $splits | where { |col| ($r | get $col) > 0 }
    if ($active_splits | length) > 1 {
      $errors = ($errors | append $"($loc): multiple active split columns: ($active_splits | str join ', ')")
    }
    if ($active_splits | is-not-empty) and ($r.kind == "income") {
      $errors = ($errors | append $"($loc): split does not apply to income")
    }
  }

  for col in $splits {
    let name = $col | str replace $SPLIT_PREFIX ""
    if (not ($name =~ '^[a-z0-9][a-z0-9-]*$')) {
      $errors = ($errors | append $"budget: invalid split column name '($col)' — name must match [a-z0-9-]+")
    }
  }

  # Reject categories that have both direct entries and children (use 'X > Y' for both).
  let categories = $budget | get category | uniq
  for cat in $categories {
    let has_children = $categories | where { |c| ($c | str starts-with $"($cat) > ") } | is-not-empty
    if $has_children {
      $errors = ($errors | append $"budget: '($cat)' has direct entries and sub-categories; use '($cat) > ...' for all entries")
    }
  }

  $errors
}
