const VALID_CADENCES = [monthly once]
const MONTH_RE = '^\d{4}-(0[1-9]|1[0-2])$'

def fin-dir []: nothing -> string {
  let d = ($env.FIN_DIR? | default "")
  if $d == "" or not ($d | path exists) {
    print $"fin2: FIN_DIR not set or not accessible" --stderr
    exit 2
  }
  $d
}

def kind-color [kind: string]: nothing -> string {
  match $kind { "income" => "green" "expense" => "red" "savings" => "blue" _ => "default" }
}

def load-categories []: nothing -> table<name: string, kind: string> {
  open ([(fin-dir) categories.csv] | path join)
    | update name { into string } | update kind { into string }
}

def load-budget []: nothing -> table {
  open ([(fin-dir) budget.csv] | path join)
    | update amount { into float } | update shared { into bool }
    | update category { into string } | update cadence { into string }
}

def rollup []: nothing -> table<category: string, kind: string, annual: float> {
  let cats = load-categories
  load-budget | each { |r|
    let annual = if $r.cadence == "monthly" { $r.amount * 12 } else { $r.amount }
    let kind = ($cats | where name == $r.category | get kind.0)
    { category: $r.category, kind: $kind, description: $r.description,
      annual: $annual, shared: $r.shared }
  }
}

def fmt [n: float]: nothing -> string {
  let sign = if $n < 0 { "-" } else { "" }
  let total_cents = (($n | math abs) * 100 | math round | into int)
  $"($sign)€(($total_cents // 100)),($total_cents mod 100 | into string | fill -a right -w 2 -c '0')"
}

def pct [part: float, total: float]: nothing -> float {
  if $total == 0 { 0.0 } else { ($part / $total * 100 | math round --precision 1) }
}

def chart [title: string, color: string] {
  $in | to tsv --noheaders | ^uplot bar -t $title -c $color -d "\t"
}

# ── Subcommands ──

def "main check" [] {
  let cats = load-categories
  let budget = load-budget
  let cat_names = $cats | get name
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
  for row in ($budget | enumerate) {
    let r = $row.item
    let n = $row.index + 2
    if $r.category not-in $cat_names {
      $errors = ($errors | append $"budget.csv:($n): unknown category '($r.category)'")
    }
    if $r.cadence not-in $VALID_CADENCES {
      $errors = ($errors | append $"budget.csv:($n): invalid cadence '($r.cadence)'")
    }
    if $r.cadence == "once" and ($r.month | is-empty) {
      $errors = ($errors | append $"budget.csv:($n): one-time entry '($r.description)' missing month")
    }
    if $r.cadence == "once" and ($r.month | is-not-empty) and ($r.month !~ $MONTH_RE) {
      $errors = ($errors | append $"budget.csv:($n): invalid month format '($r.month)' (expected YYYY-MM)")
    }
    if $r.cadence == "monthly" and ($r.month | is-not-empty) {
      $errors = ($errors | append $"budget.csv:($n): monthly entry '($r.description)' should not have a month")
    }
  }

  if ($errors | is-empty) { print "OK" } else {
    $errors | each { |e| print $"  ✗ ($e)" }
    exit 1
  }
}

def "main year" [] {
  let r = rollup
  let data = $r | group-by kind
  for kind in [income expense savings] {
    let rows = ($data | get -o $kind | default [])
    if ($rows | is-not-empty) {
      let c = kind-color $kind
      print $"\n  (ansi $c)── ($kind | str capitalize) ──(ansi reset)"
      $rows | group-by category | items { |cat, entries|
        let total = $entries | get annual | math sum
        print $"  (ansi $c)($cat | fill -w 22)(ansi reset)  (fmt $total | fill -a right -w 12)"
      }
    }
  }
  let income = ($r | where kind == "income" | get annual | math sum)
  let expense = ($r | where kind == "expense" | get annual | math sum)
  let savings = ($r | where kind == "savings" | get annual | math sum)
  print $"\n  ── Summary ──"
  print $"  Income       (fmt $income | fill -a right -w 12)"
  print $"  Expenses     (fmt $expense | fill -a right -w 12)"
  print $"  Savings      (fmt $savings | fill -a right -w 12)"
  print $"  Free         (fmt ($income - $expense - $savings) | fill -a right -w 12)"
}

def "main month" [] {
  let cats = load-categories
  load-budget | where cadence == "monthly" | each { |r|
    let kind = ($cats | where name == $r.category | get kind.0)
    { category: $r.category, kind: $kind, description: $r.description,
      amount: $r.amount, shared: $r.shared }
  } | sort-by kind category | print
}

def "main shared" [] {
  let cats = load-categories
  let rows = load-budget | where shared == true and cadence == "monthly"
  let total = $rows | get amount | math sum
  $rows | each { |r|
    let kind = ($cats | where name == $r.category | get kind.0)
    { category: $r.category, description: $r.description, amount: $r.amount, kind: $kind }
  } | sort-by category | print
  print $"\n  Transfer: (fmt $total)"
}

def "main savings" [] {
  rollup | where kind == "savings"
    | group-by category
    | items { |cat, entries| { label: $cat, value: ($entries | get annual | math sum) } }
    | sort-by value --reverse
    | chart "Savings (annual)" (kind-color savings)
}

def "main dashboard" [] {
  let r = rollup
  let income = ($r | where kind == "income" | get annual | math sum)
  let expense = ($r | where kind == "expense" | get annual | math sum)
  let savings = ($r | where kind == "savings" | get annual | math sum)
  let shared = (load-budget | where shared == true and cadence == "monthly" | get amount | math sum)

  print $"\n  (ansi green)Income(ansi reset)        (fmt $income | fill -a right -w 12)"
  print $"  (ansi red)Expenses(ansi reset)      (fmt $expense | fill -a right -w 12)   (pct $expense $income)%"
  print $"  (ansi blue)Savings(ansi reset)       (fmt $savings | fill -a right -w 12)   (pct $savings $income)%"
  print $"  Transfer/mo   (fmt $shared | fill -a right -w 12)"
  print $"  Free          (fmt ($income - $expense - $savings) | fill -a right -w 12)"

  $r | where kind == "expense"
    | group-by category
    | items { |cat, entries| { label: $cat, value: ($entries | get annual | math sum) } }
    | sort-by value --reverse
    | chart "Expenses (annual)" (kind-color expense)

  $r | where kind == "savings"
    | group-by category
    | items { |cat, entries| { label: $cat, value: ($entries | get annual | math sum) } }
    | sort-by value --reverse
    | chart "Savings (annual)" (kind-color savings)

  [[label value]; [Income $income] [Expenses $expense] [Savings $savings]]
    | chart "Annual Split" cyan
}

def "main add" [] {
  let cats = load-categories
  let budget_path = ([(fin-dir) budget.csv] | path join)
  let cat_names = $cats | get name

  let category = ($cat_names | input list "Category:")
  if ($category | is-empty) { return }

  let description = (input "Description: ")
  if ($description | is-empty) { print "Aborted."; return }

  let amount_str = (input "Amount: ")
  let amount = try { $amount_str | into float } catch { print $"Invalid amount: ($amount_str)"; return }

  let cadence = ([monthly once] | input list "Cadence:")
  if ($cadence | is-empty) { return }

  mut month = ""
  if $cadence == "once" {
    $month = (input "Month (YYYY-MM): ")
    if ($month !~ $MONTH_RE) { print $"Invalid month: ($month)"; return }
  }

  let shared = ([false true] | input list "Shared:")
  if ($shared | is-empty) { return }

  let row = [[category description amount cadence month shared]; [$category $description $amount $cadence $month $shared]]
  print $"\n  Adding: ($category) | ($description) | ($amount) | ($cadence) | ($month) | ($shared)"
  $row | to csv --noheaders | save --append --raw $budget_path
  print "  Done."
}

def "main edit" [] {
  run-external ($env.EDITOR? | default "vi") ([(fin-dir) budget.csv] | path join)
}

def main [] {
  print "fin2 <command>

  year          Annual rollup by category
  month         Monthly line items
  shared        Shared expenses (joint account transfer)
  savings       Savings breakdown
  dashboard     Visual summary
  check         Validate budget + categories
  add           Add entry (interactive, with category selection)
  edit          Open budget.csv in $EDITOR"
}
