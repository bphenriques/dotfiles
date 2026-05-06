use ./core.nu [fin-dir amt]

const VALID_CADENCES = [monthly quarterly yearly]
const WHEN_RE = '^\d{4}-(0[1-9]|1[0-2])$'
const VALID_KINDS = [income expense savings]
const SPLIT_PREFIX = "split-"

def budget-path []: nothing -> string { [(fin-dir) budget.csv] | path join }

export def split-columns []: nothing -> list<string> {
  let cols = open (budget-path) | columns
  $cols | where { |c| $c | str starts-with $SPLIT_PREFIX }
}

export def split-names []: nothing -> list<string> {
  split-columns | each { |c| $c | str replace $SPLIT_PREFIX "" }
}

def is-once [when_val: string]: nothing -> bool {
  $when_val =~ $WHEN_RE
}

export def load-budget []: nothing -> table {
  let splits = split-columns
  open (budget-path)
    | update amount { into float }
    | update category { into string }
    | update kind { into string }
    | update when { into string }
    | upsert description { |r| $r.description? | default "" | into string }
    | each { |r|
        mut row = $r
        for col in $splits {
          let raw = ($row | get $col | default "" | into string | str trim)
          $row = ($row | upsert $col (if $raw == "" { 0.0 } else { $raw | into float }))
        }
        $row
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

export def shared-monthly-transfer [split_name: string]: nothing -> float {
  let splits = split-columns
  load-budget
    | where when == "monthly"
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
    | where when == "monthly"
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

export def validate-budget []: nothing -> list<string> {
  let required = [category description amount when kind]
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
    if $r.kind not-in $VALID_KINDS {
      $errors = ($errors | append $"budget.csv:($n): invalid kind '($r.kind)'")
    }
    if $r.when not-in $VALID_CADENCES and (not (is-once $r.when)) {
      $errors = ($errors | append $"budget.csv:($n): invalid when '($r.when)' (expected monthly|quarterly|yearly|YYYY-MM)")
    }
    if ($r.description | str trim | is-empty) {
      $errors = ($errors | append $"budget.csv:($n): missing description")
    }
    if $r.amount == 0 {
      $errors = ($errors | append $"budget.csv:($n): amount must be non-zero")
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
    if ($active_splits | is-not-empty) and ($r.kind == "income") {
      $errors = ($errors | append $"budget.csv:($n): split does not apply to income")
    }
  }
  $errors
}
