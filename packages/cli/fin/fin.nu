use finlib/core.nu *
use finlib/ledger.nu *
use finlib/reports.nu *
use finlib/render.nu *

const SHOW_SECTIONS = [summary expenses savings split]
const VALID_PERIODS = [month year]

def do-show [sections: list<string>, period: string] {
  let data = dashboard-data $period
  let loc = monetary-locale
  for section in $sections {
    match $section {
      "summary" => { render-summary $data $loc }
      "expenses" => { render-expenses-table $data.expenses $loc }
      "savings" => { render-savings-chart $data.savings_rows $data.label }
      "split" => { render-split-table $data.split_rows $data.split_my_total $data.split_other_total $loc $period }
    }
  }
}

def parse-sections [input?: string]: nothing -> list<string> {
  if ($input == null) or ($input | str trim | is-empty) {
    return $SHOW_SECTIONS
  }
  let sections = $input | split row "," | each { str trim | str downcase } | where { |s| $s != "" } | uniq
  let invalid = $sections | where { |s| $s not-in $SHOW_SECTIONS }
  if ($invalid | is-not-empty) {
    print $"fin: unknown section\(s\): ($invalid | str join ', '). Choose from: ($SHOW_SECTIONS | str join ', ')" --stderr
    exit 1
  }
  $sections
}

def parse-period [input?: string]: nothing -> string {
  let p = $input | default "month" | str trim | str downcase
  if $p not-in $VALID_PERIODS {
    print $"fin: invalid period '($p)'. Choose from: ($VALID_PERIODS | str join ', ')" --stderr
    exit 1
  }
  $p
}

def do-check [] {
  let errors = validate-source

  if ($errors | is-not-empty) {
    $errors | each { |e| print $"  ✗ ($e)" --stderr }
    exit 1
  }

  run-hledger [check -s "--forecast=this year"]
  print $"(ansi green_bold)OK(ansi reset)"
}

def open-editor [csv_path: string] {
  let editor = ($env.EDITOR? | default "vi")
  ^sh -c $"($editor) \"($csv_path)\""
}

def do-edit-with-sentinel [csv_path: string, sentinel: string] {
  "" | save --force --raw $sentinel
  let err = try {
    open-editor $csv_path
    null
  } catch { |e| $e }
  rm -f $sentinel
  if $err != null { error make { msg: $"fin: editor failed: ($err.msg)" } }
}

def do-watch-dashboard [sentinel: string] {
  let render = {||
    print -e $"(ansi cursor_home)(ansi clear_entire_screen_plus_buffer)"
    try { do-show $SHOW_SECTIONS "month" } catch { |e| print $e.msg --stderr }
    print -e (ansi cursor_home)
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

def do-edit [] {
  let csv_path = [(fin-dir) budget.csv] | path join

  if ($env.ZELLIJ? | default "" | is-not-empty) or (which zellij | is-empty) {
    open-editor $csv_path
    return
  }

  let sentinel = [(runtime-dir) fin $"edit.($nu.pid).alive"] | path join
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

def do-categories [] {
  let csv_path = [(fin-dir) categories.csv] | path join
  open-editor $csv_path
}

# ── Subcommands ──

def "main show" [period?: string, sections?: string] { do-show (parse-sections $sections) (parse-period $period) }
def "main s" [period?: string, sections?: string] { do-show (parse-sections $sections) (parse-period $period) }
def "main lint" [] { do-check }
def "main l" [] { do-check }
def "main edit" [] { do-edit }
def "main e" [] { do-edit }
def "main categories" [] { do-categories }
def "main c" [] { do-categories }
def "main __edit-with-sentinel" [csv_path: string, sentinel: string] { do-edit-with-sentinel $csv_path $sentinel }
def "main __watch-dashboard" [sentinel: string] { do-watch-dashboard $sentinel }

def main [...args: string] {
  if ($args | length) > 0 and $args.0 == "--" {
    let journal = journal-path
    ^hledger -f $journal ...($args | skip 1)
  } else {
    let b = ansi white_bold
    let d = ansi white_dimmed
    let r = ansi reset
    print $"($b)fin($r) — personal finance forecast CLI \(hledger\)

  Reads budget.csv and categories.csv from FIN_DIR.

  ($b)[s]how($r) [month|year] [sections]  ($d)Show sections \(default: month, all\)
                                  Sections: summary,expenses,savings,split($r)
  ($b)[e]dit($r)                          ($d)Edit budget.csv \(live preview with zellij\)($r)
  ($b)[c]ategories($r)                    ($d)Edit categories.csv($r)
  ($b)[l]int($r)                          ($d)Validate CSVs and generated journal($r)
  ($b)-- <args>($r)                       ($d)Pass-through to hledger($r)

  ($d)Examples:($r)
    fin s                                         ($d)monthly overview($r)
    fin s year                                    ($d)annual overview($r)
    fin s month summary,expenses                  ($d)summary + expenses only($r)
    fin s year split                              ($d)annual shared breakdown($r)
    fin e                                         ($d)edit budget.csv($r)
    fin -- balance -M --depth 3 expenses:casa     ($d)ad-hoc exploration($r)
    fin -- register expenses:supermercado         ($d)transaction list($r)
    fin -- ui --forecast='this year'              ($d)launch hledger-ui($r)
    fin -- web --serve --forecast='this year'     ($d)launch hledger-web($r)"
  }
}
