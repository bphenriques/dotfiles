use finlib/core.nu *
use finlib/ledger.nu *
use finlib/reports.nu *
use finlib/render.nu *
use finlib/markdown.nu *

const SHOW_SECTIONS = [summary expenses savings split]
const VALID_PERIODS = [month year]

def maybe-sync [] {
  let md = md-source
  if $md == "" { return }
  if not ($md | path exists) {
    error make { msg: $"fin: FIN_MARKDOWN '($md)' not accessible" }
  }
  sync-md $md
}

def render-dashboard [sections: list<string>, period: string] {
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

def do-show [sections: list<string>, period: string] {
  maybe-sync
  render-dashboard $sections $period
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
  maybe-sync
  let errors = validate-source

  if ($errors | is-not-empty) {
    $errors | each { |e| print $"  ✗ ($e)" --stderr }
    exit 1
  }

  run-hledger [check -s "--forecast=this year"]
  print $"(ansi green_bold)OK(ansi reset)"
}

def do-edit [] {
  let md = md-source
  let target = if $md != "" { $md } else { (fin-dir) }
  let editor = ($env.FIN_EDITOR? | default ($env.EDITOR? | default "vi"))
  ^sh -c $"($editor) \"($target)\""
}

def do-watch [md_source: string, period: string] {
  let md_source = ($md_source | path expand)
  if not ($md_source | path exists) {
    print $"fin: not found: ($md_source)" --stderr
    exit 1
  }
  let watch_dir = if ($md_source | path type) == "dir" { $md_source } else { $md_source | path dirname }

  let render = {||
    print -e $"(ansi cursor_home)(ansi clear_entire_screen_plus_buffer)"
    try {
      sync-md $md_source
      render-dashboard $SHOW_SECTIONS $period
    } catch { |e| print $e.msg --stderr }
    print -e (ansi cursor_home)
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

def "main show" [period?: string, sections?: string] { do-show (parse-sections $sections) (parse-period $period) }
def "main s" [period?: string, sections?: string] { do-show (parse-sections $sections) (parse-period $period) }
def "main lint" [] { do-check }
def "main l" [] { do-check }
def "main edit" [] { do-edit }
def "main e" [] { do-edit }
def "main watch" [md_path?: string, --period (-p): string = "month"] {
  let path = $md_path | default (md-source)
  if $path == "" {
    print "fin: pass a markdown file or set FIN_MARKDOWN" --stderr
    exit 1
  }
  do-watch $path (parse-period $period)
}
def "main w" [md_path?: string, --period (-p): string = "month"] {
  let path = $md_path | default (md-source)
  if $path == "" {
    print "fin: pass a markdown file or set FIN_MARKDOWN" --stderr
    exit 1
  }
  do-watch $path (parse-period $period)
}

def main [...args: string] {
  if ($args | length) > 0 and $args.0 == "--" {
    maybe-sync
    let journal = journal-path
    ^hledger -f $journal ...($args | skip 1)
  } else {
    let b = ansi white_bold
    let d = ansi white_dimmed
    let r = ansi reset
    print $"($b)fin($r) — personal finance forecast CLI \(hledger\)

  Set FIN_MARKDOWN to a file or directory with <!-- fin:* --> markers.

  ($b)[s]how($r) [month|year] [sections]  ($d)Show sections \(default: month, all\)
                                  Sections: summary,expenses,savings,split($r)
  ($b)[e]dit($r)                          ($d)Open FIN_MARKDOWN or FIN_DIR in editor($r)
  ($b)[l]int($r)                          ($d)Validate CSVs and generated journal($r)
  ($b)[w]atch($r) [path] [-p period]      ($d)Watch markdown file/dir, live dashboard($r)
  ($b)-- <args>($r)                       ($d)Pass-through to hledger($r)

  ($d)Environment:($r)
    FIN_MARKDOWN                                  ($d)file or directory with <!-- fin:* --> tables($r)
    FIN_DIR                                       ($d)directory with budget.csv \(auto-generated\)($r)
    FIN_EDITOR                                    ($d)editor for fin edit \(defaults to EDITOR\)($r)

  ($d)Examples:($r)
    fin s                                         ($d)monthly overview($r)
    fin s year                                    ($d)annual overview($r)
    fin s month summary,expenses                  ($d)summary + expenses only($r)
    fin s year split                              ($d)annual shared breakdown($r)
    fin e                                         ($d)open finances in editor($r)
    fin w ~/vault/finance.md                      ($d)watch markdown, live dashboard($r)
    fin w -p year                                 ($d)watch with annual view($r)
    fin -- balance -M --depth 3 expenses:casa     ($d)ad-hoc exploration($r)
    fin -- register expenses:supermercado         ($d)transaction list($r)
    fin -- ui --forecast='this year'              ($d)launch hledger-ui($r)
    fin -- web --serve --forecast='this year'     ($d)launch hledger-web($r)"
  }
}
