use finlib/core.nu *
use finlib/budget.nu [validate-budget]
use finlib/reports.nu *
use finlib/render.nu *

const SHOW_SECTIONS = [summary expenses savings]
const VALID_PERIODS = [month year]

def render-dashboard [sections: list<string>, period: string, detailed: bool]: nothing -> bool {
  let errors = validate-budget
  if ($errors | is-not-empty) {
    print-errors $errors
    return false
  }
  let data = dashboard-data $period
  for section in $sections {
    match $section {
      "summary" => { render-summary $data }
      "expenses" => { render-expenses-table $data.expense_groups $data.income $detailed }
      "savings" => { render-savings-table $data.savings_rows $data.income $detailed }
    }
  }
  true
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

def print-errors [errors: list<string>] {
  print $"\n  (ansi red_bold)Validation errors:(ansi reset)"
  for e in $errors { print $"  (ansi red)✗(ansi reset) ($e)" }
}

def do-lint [] {
  let errors = validate-budget
  if ($errors | is-not-empty) {
    print-errors $errors
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

def do-watch [md_source: string, period: string, detailed: bool] {
  let md_source = ($md_source | path expand)
  if not ($md_source | path exists) {
    print $"fin: path not found '($md_source)'" --stderr
    exit 1
  }
  let watch_dir = if ($md_source | path type) == "dir" { $md_source } else { $md_source | path dirname }

  let render = {||
    print -e $"(ansi cursor_home)(ansi clear_entire_screen_plus_buffer)"
    try {
      render-dashboard $SHOW_SECTIONS $period $detailed
    } catch { |e| print $"\n  (ansi red_bold)Error:(ansi reset) ($e.msg)" }
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

def "main show" [sections?: string, --period (-p): string = "month", --detailed (-d)] {
  if not (render-dashboard (parse-sections $sections) (parse-period $period) $detailed) { exit 1 }
}
def "main s" [sections?: string, --period (-p): string = "month", --detailed (-d)] {
  if not (render-dashboard (parse-sections $sections) (parse-period $period) $detailed) { exit 1 }
}
def "main lint" [] { do-lint }
def "main l" [] { do-lint }
def "main edit" [] { do-edit }
def "main e" [] { do-edit }
def watch-cmd [period: string, detailed: bool, md_path?: string] {
  let path = $md_path | default (fin-dir)
  if $path == "" {
    print "fin: FIN_DIR is not set" --stderr
    exit 1
  }
  do-watch $path (parse-period $period) $detailed
}
def "main watch" [md_path?: string, --period (-p): string = "month", --detailed (-d)] { watch-cmd $period $detailed $md_path }
def "main w" [md_path?: string, --period (-p): string = "month", --detailed (-d)] { watch-cmd $period $detailed $md_path }

def main [] {
  let b = ansi white_bold
  let d = ansi white_dimmed
  let r = ansi reset
  print $"($b)fin($r) — personal finance budget CLI

  Set FIN_DIR to a file or directory with <!-- fin:budget:* --> markers.

  ($b)show($r)  [sections] [-p period] [-d] ($d)Show sections \(default: all, period: month\)
                                   Sections: summary,expenses,savings
                                   -d, --detailed: expand sub-items($r)
  ($b)edit($r)                            ($d)Open FIN_DIR in editor($r)
  ($b)lint($r)                            ($d)Validate budget($r)
  ($b)watch($r) [path] [-p period] [-d]   ($d)Watch markdown file/dir, live dashboard \(validates first\)($r)

  ($d)Environment:($r)
    FIN_DIR                                       ($d)file or directory with <!-- fin:budget:* --> tables($r)
    FIN_CURRENCY                                  ($d)currency symbol \(default €\)($r)
    FIN_EDITOR                                    ($d)editor for fin edit \(defaults to EDITOR\)($r)

  ($d)Examples:($r)
    fin s                                         ($d)monthly overview \(collapsed\)($r)
    fin s -d                                      ($d)monthly overview with sub-items($r)
    fin s -p year                                 ($d)annual overview($r)
    fin s summary,expenses                        ($d)summary + expenses only($r)
    fin e                                         ($d)open finances in editor($r)
    fin w ~/vault/finance.md                      ($d)watch markdown, live dashboard($r)
    fin w -d                                      ($d)watch with sub-items expanded($r)"
}
