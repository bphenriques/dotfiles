use finlib/core.nu *
use finlib/ledger.nu *
use finlib/reports.nu *
use finlib/render.nu *

const EDITABLE_FILES = [personal joint categories]

def do-year [] {
  run-hledger [balance -Y -E --depth 2 "--forecast=this year" -p "this year" revenues expenses "assets:savings"]
}

def do-month [] {
  run-hledger [balance -E --depth 2 "--forecast=this month" -p "this month" revenues expenses "assets:savings"]
}

def do-shared [] {
  print $"  Transfer/mo: (fmt-eur (shared-transfer))"
}

def do-savings [] {
  let raw = (run-hledger [balance -E --depth 2 "--forecast=this year" -p "this year" -O csv --layout=bare "assets:savings"])
  $raw | from csv
    | where account != "Total:"
    | each { |r| { label: ($r.account | str replace "assets:savings:" ""), value: ($r.balance | parse-balance | math round --precision 2) } }
    | sort-by value --reverse
    | chart "Savings — annual €" blue
}

def do-dashboard [period: string] {
  let data = dashboard-data $period
  render-summary $data
  render-expenses-table $data.expenses
  render-savings-chart $data.savings_rows $data.label
}

def do-check [] {
  let errors = validate-source

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
  if $err != null { error make { msg: $"fin: editor failed: ($err.msg)" } }
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
    print $"fin: unknown file '($file)'. Choose from: ($EDITABLE_FILES | str join ', ')" --stderr
    exit 1
  }
  if ($target | is-empty) { return }
  let csv_path = [(fin-dir) $"($target).csv"] | path join

  if ($env.ZELLIJ? | default "" | is-not-empty) or (which zellij | is-empty) {
    run-external ($env.EDITOR? | default "vi") $csv_path
    return
  }

  let sentinel = [(runtime-dir) fin $"edit.($target).($nu.pid).alive"] | path join
  mkdir ([(runtime-dir) fin] | path join)
  let layout = $'layout {
  default_tab_template {
    pane size=1 borderless=true {
      plugin location="zellij:compact-bar"
    }
    children
  }
  pane split_direction="vertical" {
    pane size="50%" focus=true name="Editor" command="fin" close_on_exit=true {
      args "__edit-with-sentinel" "($csv_path)" "($sentinel)"
    }
    pane size="50%" name="Dashboard" command="fin" close_on_exit=true {
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
  let journal = journal-path
  ^hledger-ui -f $journal "--forecast=this year"
}

def do-web [rest: list<string>] {
  let journal = journal-path
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
    let journal = journal-path
    ^hledger -f $journal ...($args | skip 1)
  } else {
    print "fin <command>

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
    fin d                                         annual dashboard
    fin d month                                   monthly dashboard
    fin e personal                                edit personal.csv
    fin -- balance -M --depth 3 expenses:casa     ad-hoc exploration
    fin -- register expenses:supermercado         transaction list"
  }
}
