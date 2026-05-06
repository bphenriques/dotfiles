use finlib/core.nu *
use finlib/ledger.nu *
use finlib/budget.nu [validate-budget]
use finlib/reports.nu *
use finlib/render.nu *
use finlib/markdown.nu [extract-budget]

const SHOW_SECTIONS = [summary expenses savings split]
const VALID_PERIODS = [month year]

def render-dashboard [sections: list<string>, period: string] {
  let data = dashboard-data $period
  for section in $sections {
    match $section {
      "summary" => { render-summary $data }
      "expenses" => { render-expenses-table $data.expenses }
      "savings" => { render-savings-chart $data.savings_rows $data.label }
      "split" => { render-split-table $data.split_rows $data.split_my_total $data.split_other_total }
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
    print $"fin: unknown section\(s\) '($invalid | str join ', ')'. Choose from: ($SHOW_SECTIONS | str join ', ')" --stderr
    exit 1
  }
  $sections
}

def parse-period [input?: string]: nothing -> string {
  let p = $input | default "month" | str trim | str downcase
  if $p not-in $VALID_PERIODS {
    print $"fin: invalid period '($p)'. Expected: ($VALID_PERIODS | str join ', ')" --stderr
    exit 1
  }
  $p
}

def do-lint [report?: string] {
  let errors = validate-budget
  let has_report = ($report != null) and ($report | str trim | is-not-empty)
  if $has_report {
    let dir = ($report | path dirname)
    if $dir != "" { mkdir $dir }
  }

  if ($errors | is-not-empty) {
    if $has_report {
      let items = $errors | each { |e| $"> - ($e)" } | str join "\n"
      $"# fin lint\n\n> [!error] Validation errors\n($items)\n" | save -f $report
      print $report
    } else {
      let msg = $errors | each { |e| $"  ✗ ($e)" } | str join "\n"
      print $msg --stderr
    }
    exit 1
  }

  if $has_report {
    if ($report | path exists) { rm $report }
  }

  try {
    run-hledger [check -s "--forecast=this year"]
  } catch { |e|
    if $has_report {
      $"# fin lint\n\n> [!error] hledger check failed\n> - ($e.msg)\n" | save -f $report
      print $report
    } else {
      print $e.msg --stderr
    }
    exit 1
  }

  print $"(ansi green_bold)OK(ansi reset)"
}

def do-edit [] {
  let dir = fin-dir
  if $dir == "" {
    print "fin: FIN_DIR is not set" --stderr
    exit 1
  }
  let editor = ($env.FIN_EDITOR? | default ($env.EDITOR? | default "vi"))
  ^sh -c $"($editor) \"($dir)\""
}

def do-watch [md_source: string, period: string] {
  let md_source = ($md_source | path expand)
  if not ($md_source | path exists) {
    print $"fin: path not found '($md_source)'" --stderr
    exit 1
  }
  let watch_dir = if ($md_source | path type) == "dir" { $md_source } else { $md_source | path dirname }

  let render = {||
    print -e $"(ansi cursor_home)(ansi clear_entire_screen_plus_buffer)"
    try {
      render-dashboard $SHOW_SECTIONS $period
    } catch { |e| print $e.msg --stderr }
  }

  do $render

  loop {
    let wait = (do { ^inotifywait -qq -e close_write,moved_to $watch_dir --include '.*\.md$' } | complete)
    match $wait.exit_code {
      0 => { sleep 200ms; do $render }
      _ => { break }
    }
  }
}

# ── Subcommands ──

def "main show" [sections?: string, --period (-p): string = "month"] { render-dashboard (parse-sections $sections) (parse-period $period) }
def "main s" [sections?: string, --period (-p): string = "month"] { render-dashboard (parse-sections $sections) (parse-period $period) }
def "main lint" [--report (-r): string] { do-lint $report }
def "main l" [--report (-r): string] { do-lint $report }
def "main health" [] { do-lint (health-path) }
def "main h" [] { do-lint (health-path) }
def "main edit" [] { do-edit }
def "main e" [] { do-edit }
def "main export-csv" [] {
  let dir = fin-dir
  if $dir == "" {
    print "fin: FIN_DIR is not set" --stderr
    exit 1
  }
  extract-budget $dir | to csv
}
def "main watch" [md_path?: string, --period (-p): string = "month"] {
  let path = $md_path | default (fin-dir)
  if $path == "" {
    print "fin: FIN_DIR is not set" --stderr
    exit 1
  }
  do-watch $path (parse-period $period)
}
def "main w" [md_path?: string, --period (-p): string = "month"] {
  let path = $md_path | default (fin-dir)
  if $path == "" {
    print "fin: FIN_DIR is not set" --stderr
    exit 1
  }
  do-watch $path (parse-period $period)
}

def main [...args: string] {
  if ($args | length) > 0 and $args.0 == "--" {
    let journal = journal-path
    ^hledger -f $journal ...($args | skip 1)
  } else {
    let b = ansi white_bold
    let d = ansi white_dimmed
    let r = ansi reset
    print $"($b)fin($r) — personal finance forecast CLI \(hledger\)

  Set FIN_DIR to a file or directory with <!-- fin:* --> markers.

  ($b)show($r)  [sections] [-p period]     ($d)Show sections \(default: all, period: month\)
                                  Sections: summary,expenses,savings,split($r)
  ($b)edit($r)                            ($d)Open FIN_DIR in editor($r)
  ($b)lint($r)  [-r path]                  ($d)Validate budget and generated journal($r)
  ($b)health($r)                          ($d)Lint with report to FIN_HEALTH($r)
  ($b)watch($r) [path] [-p period]        ($d)Watch markdown file/dir, live dashboard($r)
  ($b)export-csv($r)                      ($d)Export budget as CSV to stdout($r)
  ($b)-- <args>($r)                       ($d)Pass-through to hledger($r)

  ($d)Environment:($r)
    FIN_DIR                                       ($d)file or directory with <!-- fin:* --> tables($r)
    FIN_EDITOR                                    ($d)editor for fin edit \(defaults to EDITOR\)($r)
    FIN_HEALTH                                    ($d)path to health report \(default: XDG_RUNTIME_DIR/fin/fin-health.md\)($r)

  ($d)Examples:($r)
    fin s                                         ($d)monthly overview($r)
    fin s -p year                                 ($d)annual overview($r)
    fin s summary,expenses                        ($d)summary + expenses only($r)
    fin s expenses                                ($d)expenses only($r)
    fin s split -p year                           ($d)annual shared breakdown($r)
    fin e                                         ($d)open finances in editor($r)
    fin w ~/vault/finance.md                      ($d)watch markdown, live dashboard($r)
    fin w -p year                                 ($d)watch with annual view($r)
    fin export-csv                                ($d)export budget as CSV($r)
    fin -- balance -M --depth 3 expenses:casa     ($d)ad-hoc exploration($r)
    fin -- register expenses:supermercado         ($d)transaction list($r)
    fin -- ui --forecast='this year'              ($d)launch hledger-ui($r)
    fin -- web --serve --forecast='this year'     ($d)launch hledger-web($r)"
  }
}
