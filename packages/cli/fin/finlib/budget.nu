use ./core.nu [fin-dir amt MONTH_RE]

const VALID_CADENCES = [monthly quarterly yearly once]
const SPLIT_PREFIX = "split-"

def budget-path []: nothing -> string { [(fin-dir) budget.csv] | path join }
def categories-path []: nothing -> string { [(fin-dir) categories.csv] | path join }

export def load-categories []: nothing -> table<name: string, kind: string> {
  open (categories-path)
    | update name { into string } | update kind { into string }
}

# Returns split column names found in the budget (e.g., ["split-joint"]).
export def split-columns []: nothing -> list<string> {
  let cols = open (budget-path) | columns
  $cols | where { |c| $c | str starts-with $SPLIT_PREFIX }
}

# Returns split names without prefix (e.g., ["joint"]).
export def split-names []: nothing -> list<string> {
  split-columns | each { |c| $c | str replace $SPLIT_PREFIX "" }
}

export def load-budget []: nothing -> table {
  let splits = split-columns
  open (budget-path)
    | update amount { into float } | update category { into string }
    | update cadence { into string }
    | upsert sub { |r| $r.sub? | default "" | into string }
    | upsert month { |r| $r.month? | default "" | into string }
    | each { |r|
        mut row = $r
        for col in $splits {
          let raw = ($row | get $col | default "" | into string | str trim)
          $row = ($row | upsert $col (if $raw == "" { 0.0 } else { $raw | into float }))
        }
        $row
      }
}

# Returns the split name and weight for a row, or {name: null, weight: 1.0} if personal.
export def row-split [row: record, splits: list<string>]: nothing -> record {
  for col in $splits {
    let val = ($row | get $col)
    if $val > 0 {
      return { name: ($col | str replace $SPLIT_PREFIX ""), weight: $val }
    }
  }
  { name: null, weight: 1.0 }
}

export def enrich-budget [cats: table]: table -> table {
  let cat_names = $cats | get name
  let splits = split-columns
  $in | each { |row|
    if $row.category not-in $cat_names {
      error make { msg: $"fin: unknown category '($row.category)' in budget.csv" }
    }
    if $row.cadence not-in $VALID_CADENCES {
      error make { msg: $"fin: invalid cadence '($row.cadence)' in budget.csv" }
    }
    let kind = ($cats | where name == $row.category | get kind.0)
    let split = row-split $row $splits
    {
      row: $row,
      kind: $kind,
      split_name: $split.name,
      effective_amount: ($row.amount * $split.weight | amt)
    }
  }
}

export def shared-monthly-transfer [split_name: string]: nothing -> float {
  let splits = split-columns
  load-budget
    | where cadence == "monthly"
    | each { |r|
        let split = row-split $r $splits
        if $split.name == $split_name {
          $r.amount * $split.weight | amt
        } else { 0 }
      }
    | math sum
}

export def shared-monthly-breakdown [split_name: string]: nothing -> table {
  let splits = split-columns
  load-budget
    | where cadence == "monthly"
    | each { |r|
        let split = row-split $r $splits
        if $split.name == $split_name {
          {
            group: $split_name,
            description: ($r.description? | default "" | into string),
            amount: $r.amount,
            share: $split.weight,
            contribution: ($r.amount * $split.weight | amt)
          }
        }
      }
    | compact
    | sort-by contribution --reverse
}

export def validate-categories [cats: table]: nothing -> list<string> {
  mut errors: list<string> = []
  let dup_cats = ($cats | get name | uniq --repeated)
  if ($dup_cats | is-not-empty) {
    $errors = ($errors | append $"categories.csv: duplicate categories: ($dup_cats | str join ', ')")
  }
  for row in $cats {
    if $row.kind not-in [income expense savings] {
      $errors = ($errors | append $"categories.csv: '($row.name)' has invalid kind '($row.kind)'")
    }
  }
  $errors
}

export def validate-budget [cats: table]: nothing -> list<string> {
  let cat_names = $cats | get name
  let required = [category description amount cadence]
  let cols = open (budget-path) | columns
  let missing = $required | where { |c| $c not-in $cols }
  if ($missing | is-not-empty) {
    return [$"budget.csv: missing required columns: ($missing | str join ', ')"]
  }

  let splits = split-columns
  let budget = load-budget
  mut errors: list<string> = []
  for row in ($budget | enumerate) {
    let r = $row.item
    let n = $row.index + 2
    if $r.category not-in $cat_names {
      $errors = ($errors | append $"budget.csv:($n): unknown category '($r.category)'")
    }
    if $r.cadence not-in $VALID_CADENCES {
      $errors = ($errors | append $"budget.csv:($n): invalid cadence '($r.cadence)'")
    }
    if ($r.description? | default "" | into string | str trim | is-empty) {
      $errors = ($errors | append $"budget.csv:($n): missing description")
    }
    if $r.amount == 0 {
      $errors = ($errors | append $"budget.csv:($n): amount must be non-zero")
    }
    let sub = ($r.sub? | default "" | into string)
    if ($sub | str starts-with ":") or ($sub | str ends-with ":") or ($sub | str contains "::") {
      $errors = ($errors | append $"budget.csv:($n): sub '($sub)' has invalid ':' placement")
    }
    if $r.cadence == "once" {
      let month = ($r.month? | default "" | into string)
      if ($month | is-empty) {
        $errors = ($errors | append $"budget.csv:($n): one-time entry '($r.description)' missing month")
      } else if ($month !~ $MONTH_RE) {
        $errors = ($errors | append $"budget.csv:($n): invalid month format '($month)' (expected YYYY-MM)")
      }
    }
    for col in $splits {
      let val = ($r | get $col)
      if $val < 0 or $val > 1 {
        $errors = ($errors | append $"budget.csv:($n): ($col) '($val)' must be between 0 and 1")
      }
    }
    let active_splits = $splits | where { |col| ($r | get $col) > 0 }
    if ($active_splits | length) > 1 {
      $errors = ($errors | append $"budget.csv:($n): multiple active split columns: ($active_splits | str join ', ')")
    }
    if ($active_splits | is-not-empty) {
      let kind = ($cats | where name == $r.category | get kind | first)
      if $kind == "income" {
        $errors = ($errors | append $"budget.csv:($n): split columns do not apply to income categories, but '($r.category)' is income")
      }
    }
  }
  $errors
}
