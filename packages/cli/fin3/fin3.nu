const VALID_CADENCES = [monthly quarterly yearly once]
const MONTH_RE = '^\d{4}-(0[1-9]|1[0-2])$'
const KIND_PREFIX = { income: "revenues", expense: "expenses", savings: "assets:savings" }
const PERSONAL_ACCOUNT = "assets:banco:conta-pessoal"
const JOINT_ACCOUNT = "assets:banco:conta-conjunta"

def fin-dir []: nothing -> string {
  let d = ($env.FIN_DIR? | default "")
  if $d == "" or not ($d | path exists) {
    print "fin3: FIN_DIR not set or not accessible" --stderr
    exit 2
  }
  $d
}

def runtime-dir []: nothing -> string {
  let d = ($env.XDG_RUNTIME_DIR? | default "")
  if $d == "" {
    error make { msg: "fin3: XDG_RUNTIME_DIR is not set" }
  }
  $d
}

def load-categories []: nothing -> table<name: string, kind: string> {
  open ([(fin-dir) categories.csv] | path join)
    | update name { into string } | update kind { into string }
}

def load-budget [file: string]: nothing -> table {
  open ([(fin-dir) $file] | path join)
    | update amount { into float } | update category { into string }
    | update cadence { into string }
}

def enrich-budget [cats: table, file: string, bank: string]: table -> table {
  let cat_names = $cats | get name
  $in | each { |row|
    if $row.category not-in $cat_names {
      error make { msg: $"fin3: unknown category '($row.category)' in ($file)" }
    }
    if $row.cadence not-in $VALID_CADENCES {
      error make { msg: $"fin3: invalid cadence '($row.cadence)' in ($file)" }
    }
    let kind = ($cats | where name == $row.category | get kind.0)
    { row: $row, kind: $kind, bank: $bank }
  }
}

def prettify-slug []: string -> string {
  split row "-" | each { str capitalize } | str join " "
}

def derive-account [category: string, sub: string, kind: string]: nothing -> string {
  let prefix = ($KIND_PREFIX | get $kind)
  if ($sub | is-empty) { $"($prefix):($category)" } else { $"($prefix):($category):($sub)" }
}

def fmt-eur [n: float]: nothing -> string {
  let sign = if $n < 0 { "-" } else { "" }
  let total_cents = (($n | math abs) * 100 | math round | into int)
  let whole = $total_cents // 100
  let frac = ($total_cents mod 100 | into string | fill -a right -w 2 -c '0')
  let whole_str = if $whole >= 1_000_000 {
    let m = $whole // 1_000_000
    let k = ($whole mod 1_000_000 // 1000 | into string | fill -a right -w 3 -c '0')
    let u = ($whole mod 1000 | into string | fill -a right -w 3 -c '0')
    $"($m).($k).($u)"
  } else if $whole >= 1000 {
    let k = $whole // 1000
    let u = ($whole mod 1000 | into string | fill -a right -w 3 -c '0')
    $"($k).($u)"
  } else {
    $whole | into string
  }
  $"($sign)€($whole_str),($frac)"
}

def pct [part: float, total: float]: nothing -> float {
  if $total == 0 { 0.0 } else { ($part / $total * 100 | math round --precision 1) }
}

def cadence-rule [cadence: string]: nothing -> string {
  match $cadence {
    "monthly" => "monthly"
    "quarterly" => "every 3 months"
    "yearly" => "yearly"
    _ => { error make { msg: $"fin3: unsupported cadence '($cadence)'" } }
  }
}

def monthly-joint-transfer []: nothing -> float {
  load-budget "joint.csv" | update my_share { into float }
    | where cadence == "monthly"
    | each { |r| $r.amount * $r.my_share }
    | append 0 | math sum | math round --precision 2
}

def render-row [
  row: record
  kind: string
  bank_account: string
  amount: float
]: nothing -> string {
  let account = derive-account $row.category ($row.sub? | default "") $kind
  if $row.cadence == "once" {
    if ($row.month | is-empty) or ($row.month !~ $MONTH_RE) {
      error make { msg: $"fin3: once entry '($row.description)' has invalid month '($row.month? | default "")'" }
    }
    let date = $"($row.month)-01"
    if $kind == "income" {
      $"($date) ($row.description)\n    ($bank_account)    (fmt-eur $amount)\n    ($account)\n"
    } else {
      $"($date) ($row.description)\n    ($account)    (fmt-eur $amount)\n    ($bank_account)\n"
    }
  } else {
    let rule = cadence-rule $row.cadence
    if $kind == "income" {
      $"~ ($rule)  ($row.description)\n    ($bank_account)    (fmt-eur $amount)\n    ($account)\n"
    } else {
      $"~ ($rule)  ($row.description)\n    ($account)    (fmt-eur $amount)\n    ($bank_account)\n"
    }
  }
}

def generate-journal []: nothing -> string {
  let cats = load-categories

  let personal = load-budget "personal.csv"
    | upsert sub { |r| $r.sub? | default "" | into string }
    | upsert month { |r| $r.month? | default "" | into string }
  let joint = load-budget "joint.csv"
    | upsert sub { |r| $r.sub? | default "" | into string }
    | upsert month { |r| $r.month? | default "" | into string }
    | update my_share { into float }

  let personal_enriched = $personal | enrich-budget $cats "personal.csv" $PERSONAL_ACCOUNT
  let joint_enriched = $joint | enrich-budget $cats "joint.csv" $JOINT_ACCOUNT

  let personal_periodic = $personal_enriched | where row.cadence != "once"
  let personal_once = $personal_enriched | where row.cadence == "once"
  let joint_periodic = $joint_enriched | where row.cadence != "once"
  let joint_once = $joint_enriched | where row.cadence == "once"

  let all_accounts = ($personal_enriched | each { |e| derive-account $e.row.category ($e.row.sub? | default "") $e.kind })
    | append ($joint_enriched | each { |e| derive-account $e.row.category ($e.row.sub? | default "") $e.kind })
    | append [$PERSONAL_ACCOUNT $JOINT_ACCOUNT]
    | uniq | sort

  mut parts: list<string> = [
    $"; Generated by fin3 — do not edit"
    $"; Source: (fin-dir)"
    ""
    "commodity €1.000,00"
    ""
    "account assets      ; type: A"
    "account revenues    ; type: R"
    "account expenses    ; type: X"
  ]
  for acc in $all_accounts {
    $parts = ($parts | append $"account ($acc)")
  }

  if ($personal_periodic | is-not-empty) {
    $parts = ($parts | append ["" "; ── Personal ──" ""])
    for entry in $personal_periodic {
      $parts = ($parts | append (render-row $entry.row $entry.kind $entry.bank $entry.row.amount))
    }
  }

  if ($joint_periodic | is-not-empty) {
    $parts = ($parts | append ["" "; ── Joint ──" ""])
    for entry in $joint_periodic {
      let amount = ($entry.row.amount * $entry.row.my_share | math round --precision 2)
      $parts = ($parts | append (render-row $entry.row $entry.kind $entry.bank $amount))
    }
  }

  let transfer_total = monthly-joint-transfer
  if $transfer_total > 0 {
    $parts = ($parts | append [
      ""
      "; ── Transfer ──"
      ""
      $"~ monthly  Transferência conta conjunta"
      $"    ($JOINT_ACCOUNT)    (fmt-eur $transfer_total)"
      $"    ($PERSONAL_ACCOUNT)"
      ""
    ])
  }

  if ($personal_once | is-not-empty) or ($joint_once | is-not-empty) {
    $parts = ($parts | append ["" "; ── One-off ──" ""])
    for entry in $personal_once {
      $parts = ($parts | append (render-row $entry.row $entry.kind $entry.bank $entry.row.amount))
    }
    for entry in $joint_once {
      let amount = ($entry.row.amount * $entry.row.my_share | math round --precision 2)
      $parts = ($parts | append (render-row $entry.row $entry.kind $entry.bank $amount))
    }
  }

  $parts | str join "\n"
}

def ensure-journal []: nothing -> string {
  let dir = [(runtime-dir) fin3] | path join
  let path = [$dir plan.generated.journal] | path join
  let content = generate-journal

  mkdir $dir
  let existing = if ($path | path exists) { open $path --raw } else { "" }
  if $content != $existing { $content | save --force --raw $path }
  $path
}

def run-hledger [args: list<string>] {
  let journal = ensure-journal
  ^hledger -f $journal ...$args
}

def chart [title: string, color: string] {
  $in | to tsv --noheaders | ^uplot bar -t $title -c $color -d "\t"
}

def parse-balance []: string -> float {
  str replace --all '.' '' | str replace ',' '.' | into float
}

const EDITABLE_FILES = [personal joint categories]

def do-year [] {
  run-hledger [balance -Y -E --depth 2 "--forecast=this year" -p "this year" revenues expenses "assets:savings"]
}

def do-month [] {
  run-hledger [balance -E --depth 2 "--forecast=this month" -p "this month" revenues expenses "assets:savings"]
}

def do-shared [] {
  print $"  Transfer/mo: (fmt-eur (monthly-joint-transfer))"
}

def do-savings [] {
  let raw = (run-hledger [balance -E --depth 2 "--forecast=this year" -p "this year" -O csv --layout=bare "assets:savings"])
  $raw | from csv
    | where account != "Total:"
    | each { |r| { label: ($r.account | str replace "assets:savings:" ""), value: ($r.balance | parse-balance | math round --precision 2) } }
    | sort-by value --reverse
    | chart "Savings — annual €" blue
}

def leaf-accounts []: table -> table {
  let data = $in
  $data | where { |r| ($data | where { |o| $o.account | str starts-with $"($r.account):" } | is-empty) }
}

def dashboard-data [period: string]: nothing -> record {
  let journal = ensure-journal
  let is_month = $period == "month"
  let forecast = if $is_month { "--forecast=this month" } else { "--forecast=this year" }
  let time = if $is_month { "this month" } else { "this year" }
  let label = if $is_month { "monthly €" } else { "annual €" }

  # Summary totals — let hledger aggregate at depth 1 (no leaf computation)
  let totals = (^hledger -f $journal balance -E --depth 1 $forecast -p $time -O csv --layout=bare revenues expenses)
    | from csv | where account != "Total:"
  let income = ($totals | where account == "revenues" | get balance | append "0" | first | parse-balance | math abs | math round --precision 2)
  let expense = ($totals | where account == "expenses" | get balance | append "0" | first | parse-balance | math abs | math round --precision 2)

  let savings = (^hledger -f $journal balance -E --depth 2 $forecast -p $time -O csv --layout=bare "assets:savings")
    | from csv | where account == "assets:savings" | get balance | append "0" | first | parse-balance | math abs | math round --precision 2

  let transfer = monthly-joint-transfer

  # Expense breakdown
  let exp_rows = (^hledger -f $journal balance -E --depth 3 $forecast -p $time -O csv --layout=bare expenses)
    | from csv | where account != "Total:" and account != "expenses" | leaf-accounts
    | each { |r|
      let v = ($r.balance | parse-balance | math round --precision 2)
      let parts = ($r.account | str replace "expenses:" "" | split row ":")
      { category: ($parts | first | prettify-slug), sub: (if ($parts | length) > 1 { $parts | skip 1 | each { prettify-slug } | str join " " } else { "" }), amount: $v }
    } | sort-by amount --reverse

  # Savings breakdown
  let sav_rows = (^hledger -f $journal balance -E --depth 3 $forecast -p $time -O csv --layout=bare "assets:savings")
    | from csv | where account != "Total:" and account != "assets:savings" | leaf-accounts
    | each { |r|
      let v = ($r.balance | parse-balance | math round --precision 2)
      { label: ($r.account | str replace "assets:savings:" "" | prettify-slug), value: $v, pct: (pct $v $savings) }
    } | sort-by value --reverse

  { income: $income, expense: $expense, savings: $savings, transfer: $transfer, free: ($income - $expense - $savings | math round --precision 2), label: $label, expenses: $exp_rows, savings_rows: $sav_rows }
}

def render-summary [data: record] {
  print $"\n  (ansi green)Income(ansi reset)        (fmt-eur $data.income | fill -a right -w 12)"
  print $"  (ansi red)Expenses(ansi reset)      (fmt-eur $data.expense | fill -a right -w 12)   (pct $data.expense $data.income)%"
  print $"  (ansi blue)Savings(ansi reset)       (fmt-eur $data.savings | fill -a right -w 12)   (pct $data.savings $data.income)%"
  print $"  Transfer/mo   (fmt-eur $data.transfer | fill -a right -w 12)"
  print $"  Free          (fmt-eur $data.free | fill -a right -w 12)"
}

def render-expenses-table [rows: table] {
  print ""
  let cat_hdr = $"(ansi red)Category(ansi reset)"
  let sub_hdr = $"(ansi red)Description(ansi reset)"
  let amt_hdr = $"(ansi red)Amount(ansi reset)"
  $rows
    | each { |r| { sort: $r.amount, $cat_hdr: $r.category, $sub_hdr: $r.sub, $amt_hdr: (fmt-eur $r.amount) } }
    | sort-by sort --reverse
    | reject sort
    | table --index false
    | print
}

def render-savings-chart [rows: table, label: string] {
  $rows
    | each { |r| { label: $"($r.label) \(($r.pct)%\)", value: $r.value } }
    | chart $"Savings — ($label)" blue
}

def do-dashboard [period: string] {
  let data = dashboard-data $period
  render-summary $data
  render-expenses-table $data.expenses
  render-savings-chart $data.savings_rows $data.label
}

def validate-categories [cats: table]: nothing -> list<string> {
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

def validate-budget [cat_names: list<string>, file: string]: nothing -> list<string> {
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

def do-check [] {
  let cats = load-categories
  let errors = (validate-categories $cats)
    | append (validate-budget ($cats | get name) "personal.csv")
    | append (validate-budget ($cats | get name) "joint.csv")

  if ($errors | is-not-empty) {
    $errors | each { |e| print $"  ✗ ($e)" --stderr }
    exit 1
  }

  run-hledger [check -s "--forecast=this year"]
  print "OK"
}

def do-edit-with-sentinel [csv_path: string, sentinel: string] {
  "" | save --force --raw $sentinel
  let err = try {
    run-external ($env.EDITOR? | default "vi") $csv_path
    null
  } catch { |e| $e }
  rm -f $sentinel
  if $err != null { error make { msg: $"fin3: editor failed: ($err.msg)" } }
}

def do-watch-dashboard [sentinel: string] {
  let render = {||
    print -e "\e[H\e[2J\e[3J"
    try { do-dashboard "year" } catch { |e| print $e.msg --stderr }
    print -e "\e[H"
  }

  if not ($sentinel | path exists) { return }
  do $render

  loop {
    if not ($sentinel | path exists) { break }
    let wait = (do { ^inotifywait -qq -t 2 -e close_write,moved_to (fin-dir) } | complete)
    if not ($sentinel | path exists) { break }
    match $wait.exit_code {
      0 => { sleep 200ms; do $render }
      2 => { }
      _ => { break }
    }
  }
}

def do-edit [file?: string] {
  let target = if ($file == null) {
    $EDITABLE_FILES | input list "Edit:"
  } else if ($file in $EDITABLE_FILES) {
    $file
  } else {
    print $"fin3: unknown file '($file)'. Choose from: ($EDITABLE_FILES | str join ', ')" --stderr
    exit 1
  }
  if ($target | is-empty) { return }
  let csv_path = [(fin-dir) $"($target).csv"] | path join

  if ($env.ZELLIJ? | default "" | is-not-empty) or (which zellij | is-empty) {
    run-external ($env.EDITOR? | default "vi") $csv_path
    return
  }

  let sentinel = [(runtime-dir) fin3 $"edit.($target).($nu.pid).alive"] | path join
  mkdir ([(runtime-dir) fin3] | path join)
  let layout = $'layout {
  default_tab_template {
    pane size=1 borderless=true {
      plugin location="zellij:compact-bar"
    }
    children
  }
  pane split_direction="vertical" {
    pane size="50%" focus=true name="Editor" command="fin3" close_on_exit=true {
      args "__edit-with-sentinel" "($csv_path)" "($sentinel)"
    }
    pane size="50%" name="Dashboard" command="fin3" close_on_exit=true {
      args "__watch-dashboard" "($sentinel)"
    }
  }
}
pane_frames false
default_mode "locked"'
  ^zellij --layout-string $layout
  rm -f $sentinel
}

def do-ui [] {
  let journal = ensure-journal
  ^hledger-ui -f $journal "--forecast=this year"
}

def do-web [rest: list<string>] {
  let journal = ensure-journal
  ^hledger-web -f $journal --serve "--forecast=this year" ...$rest
}

# ── Subcommands ──

def "main year" [] { do-year }
def "main y" [] { do-year }
def "main month" [] { do-month }
def "main m" [] { do-month }
def "main shared" [] { do-shared }
def "main s" [] { do-shared }
def "main savings" [] { do-savings }
def "main v" [] { do-savings }
def "main dashboard" [period?: string] { do-dashboard ($period | default "year") }
def "main d" [period?: string] { do-dashboard ($period | default "year") }
def "main check" [] { do-check }
def "main c" [] { do-check }
def "main edit" [file?: string] { do-edit $file }
def "main e" [file?: string] { do-edit $file }
def "main ui" [] { do-ui }
def "main u" [] { do-ui }
def "main web" [...rest: string] { do-web $rest }
def "main w" [...rest: string] { do-web $rest }
def "main __edit-with-sentinel" [csv_path: string, sentinel: string] { do-edit-with-sentinel $csv_path $sentinel }
def "main __watch-dashboard" [sentinel: string] { do-watch-dashboard $sentinel }

def main [...args: string] {
  if ($args | length) > 0 and $args.0 == "--" {
    let journal = ensure-journal
    ^hledger -f $journal ...($args | skip 1)
  } else {
    print "fin3 <command>

  [y]ear          Annual forecast
  [m]onth         Current month forecast
  [s]hared        Joint account transfer amount
  sa[v]ings       Savings breakdown chart
  [d]ashboard [month|year]  Visual summary with charts (default: year)
  [c]heck         Validate CSVs and generated journal
  [e]dit [file]   Edit CSV with live dashboard (uses zellij if available)
  [u]i            Launch hledger-ui
  [w]eb           Launch hledger-web
  -- <args>       Pass-through to hledger

  Examples:
    fin3 d                                         annual dashboard
    fin3 d month                                   monthly dashboard
    fin3 e personal                                edit personal.csv
    fin3 -- balance -M --depth 3 expenses:casa     ad-hoc exploration
    fin3 -- register expenses:supermercado         transaction list"
  }
}
