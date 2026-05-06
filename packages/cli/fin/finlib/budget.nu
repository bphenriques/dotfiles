use ./core.nu [fin-dir amt FREQUENCY_RE MONTHLY_DIVISOR category-to-account]
use ./markdown.nu [extract-budget]

const VALID_CADENCES = [monthly quarterly yearly]
const VALID_KINDS = [income expense savings]
const SPLIT_PREFIX = "split-"

export def split-columns []: nothing -> list<string> {
  extract-budget (fin-dir) | columns | where { |c| $c | str starts-with $SPLIT_PREFIX }
}

export def split-names []: nothing -> list<string> {
  split-columns | each { |c| $c | str replace $SPLIT_PREFIX "" }
}

export def load-budget []: nothing -> table {
  let raw = extract-budget (fin-dir)
  let splits = $raw | columns | where { |c| $c | str starts-with $SPLIT_PREFIX }
  $raw | each { |r|
    let amt = try { $r.amount | into float } catch { error make { msg: $"budget: invalid amount '($r.amount)' in category '($r.category)'" } }
    let base = $r
      | upsert amount $amt
      | upsert category ($r.category | into string)
      | upsert kind ($r.kind | into string)
      | upsert frequency ($r.frequency | into string)
      | upsert description ($r.description? | default "" | into string)
    $splits | reduce -f $base { |col, acc|
      let v = ($acc | get $col | default "" | into string | str trim)
      let parsed = if $v == "" { 0.0 } else { try { $v | into float } catch { error make { msg: $"budget: invalid split value '($v)' in category '($r.category)'" } } }
      $acc | upsert $col $parsed
    }
  }
}

export def row-split [row: record, splits: list<string>]: nothing -> record {
  for col in $splits {
    let val = ($row | get $col)
    if $val > 0 {
      return { name: ($col | str replace $SPLIT_PREFIX ""), weight: $val }
    }
  }
  { name: null, weight: 1.0 }
}

export def enrich-budget []: table -> table {
  let splits = split-columns
  $in | each { |row|
    let split = row-split $row $splits
    {
      row: $row,
      kind: $row.kind,
      split_name: $split.name,
      effective_amount: ($row.amount * $split.weight | amt)
    }
  }
}

# Returns one row per split name with the normalized monthly transfer total.
# monthly amounts as-is, quarterly / 3, yearly / 12.
export def split-monthly-transfer []: nothing -> table {
  let splits = split-columns
  let entries = load-budget
    | where { |r| $r.frequency in $VALID_CADENCES }
    | each { |r|
        let split = row-split $r $splits
        if $split.name != null {
          let divisor = $MONTHLY_DIVISOR | get $r.frequency
          { split_name: $split.name, amount: ($r.amount * $split.weight / $divisor | amt) }
        }
      }
    | compact
  let names = $entries | get split_name | uniq
  $names | each { |name|
    let total = $entries | where split_name == $name | get amount | math sum | amt
    { split_name: $name, total: $total }
  } | where total > 0
}

export def split-breakdown []: nothing -> table {
  let splits = split-columns
  load-budget
    | each { |r|
        let split = row-split $r $splits
        if $split.name != null {
          {
            group: $split.name,
            description: ($r.description? | default "" | into string),
            amount: $r.amount,
            share: $split.weight,
            contribution: ($r.amount * $split.weight | amt),
            frequency: $r.frequency
          }
        }
      }
    | compact
    | sort-by contribution --reverse
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
    let loc = $"($r._file | path basename):($r._line)"
    if $r.kind not-in $VALID_KINDS {
      $errors = ($errors | append $"($loc): invalid kind '($r.kind)'")
    }
    if $r.frequency not-in $VALID_CADENCES and (not ($r.frequency =~ $FREQUENCY_RE)) {
      $errors = ($errors | append $"($loc): invalid frequency '($r.frequency)' (expected monthly|quarterly|yearly|YYYY-MM)")
    }
    if ($r.category | str trim | is-empty) {
      $errors = ($errors | append $"($loc): missing category")
    }
    if ($r.description | str trim | is-empty) {
      $errors = ($errors | append $"($loc): missing description")
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

  # Validate split column names (must be valid slug for bank account derivation)
  for col in $splits {
    let name = $col | str replace $SPLIT_PREFIX ""
    if (not ($name =~ '^[a-z0-9][a-z0-9-]*$')) {
      $errors = ($errors | append $"budget: invalid split column name '($col)' — name must match [a-z0-9-]+")
    }
  }

  # Check for slug collisions (different categories mapping to same account)
  let accounts = $budget | where { |r| $r.kind in $VALID_KINDS } | each { |r| { account: (category-to-account $r.category $r.kind), category: $r.category } }
  let seen = $accounts | group-by account
  for group in ($seen | transpose key value) {
    let cats = $group.value | get category | uniq
    if ($cats | length) > 1 {
      $errors = ($errors | append $"budget: slug collision: ($cats | str join ', ') → ($group.key)")
    }
  }

  # Check for categories that have both direct entries and children
  let categories = $budget | get category | uniq
  for cat in $categories {
    let has_children = $categories | where { |c| ($c | str starts-with $"($cat) > ") } | is-not-empty
    if $has_children {
      $errors = ($errors | append $"budget: '($cat)' has direct entries and sub-categories; use '($cat) > ...' for all entries")
    }
  }

  $errors
}
