use ./core.nu [fin-dir MONTH_RE]

const VALID_CADENCES = [monthly quarterly yearly once]

export def load-categories []: nothing -> table<name: string, kind: string> {
  open ([(fin-dir) categories.csv] | path join)
    | update name { into string } | update kind { into string }
}

export def load-budget [file: string]: nothing -> table {
  open ([(fin-dir) $file] | path join)
    | update amount { into float } | update category { into string }
    | update cadence { into string }
}

export def enrich-budget [cats: table, file: string, bank: string]: table -> table {
  let cat_names = $cats | get name
  $in | each { |row|
    if $row.category not-in $cat_names {
      error make { msg: $"fin: unknown category '($row.category)' in ($file)" }
    }
    if $row.cadence not-in $VALID_CADENCES {
      error make { msg: $"fin: invalid cadence '($row.cadence)' in ($file)" }
    }
    let kind = ($cats | where name == $row.category | get kind.0)
    { row: $row, kind: $kind, bank: $bank }
  }
}

export def monthly-joint-transfer []: nothing -> float {
  load-budget "joint.csv" | update my_share { into float }
    | where cadence == "monthly"
    | each { |r| $r.amount * $r.my_share }
    | append 0 | math sum | math round --precision 2
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

export def validate-budget [cat_names: list<string>, file: string]: nothing -> list<string> {
  let budget = load-budget $file
  mut errors: list<string> = []
  for row in ($budget | enumerate) {
    let r = $row.item
    let n = $row.index + 2
    if $r.category not-in $cat_names {
      $errors = ($errors | append $"($file):($n): unknown category '($r.category)'")
    }
    if $r.cadence not-in $VALID_CADENCES {
      $errors = ($errors | append $"($file):($n): invalid cadence '($r.cadence)'")
    }
    if $r.cadence == "once" {
      let month = ($r.month? | default "" | into string)
      if ($month | is-empty) {
        $errors = ($errors | append $"($file):($n): one-time entry '($r.description)' missing month")
      } else if ($month !~ $MONTH_RE) {
        $errors = ($errors | append $"($file):($n): invalid month format '($month)' (expected YYYY-MM)")
      }
    }
  }
  $errors
}
