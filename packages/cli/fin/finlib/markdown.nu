use ./core.nu [fin-dir]

def csv-escape-cell []: string -> string {
  let v = $in
  let dq = '"'
  if ($v | str contains ",") or ($v | str contains $dq) or ($v | str contains "\n") {
    let escaped = $v | str replace --all $dq '""'
    $'($dq)($escaped)($dq)'
  } else {
    $v
  }
}

def normalize-decimal []: string -> string {
  let v = $in
  if ($v =~ '^-?\d+,\d{1,2}$') { $v | str replace ',' '.' } else { $v }
}

def slugify []: string -> string {
  str downcase | str trim | str replace --all ' ' '-'
}

def parse-md-row []: string -> list<string> {
  str trim
    | str replace --regex '^\|' ''
    | str replace --regex '\|$' ''
    | split row '|'
    | each { str trim | str replace --all '**' '' | normalize-decimal }
}

def row-to-csv [cells: list<string>]: nothing -> string {
  $cells | each { csv-escape-cell } | str join ","
}

def extract-budget-tables [content: string]: nothing -> list<record<header: list<string>, rows: list<list<string>>, suffix: string>> {
  let lines = $content | lines
  let marker_re = "<!-- fin:budget:"
  let markers = $lines | enumerate | where { |e| ($e.item | str trim | str starts-with $marker_re) }

  $markers | each { |m|
    let marker_text = $m.item | str trim
    let suffix = $marker_text | str replace "<!-- fin:budget:" "" | str replace " -->" ""
    let after = $lines | skip ($m.index + 1) | where { |l| ($l | str trim) != "" }
    let table_lines = $after | take while { |l| ($l | str trim | str starts-with "|") }
    if ($table_lines | length) < 2 { return [] }
    let header = $table_lines | first | parse-md-row | each { slugify }
    let rows = $table_lines | skip 2 | each { |r| $r | parse-md-row }
    { header: $header, rows: $rows, suffix: $suffix }
  } | where { |t| ($t | is-not-empty) and ($t.rows | is-not-empty) }
}

def table-to-csv [header: list<string>, rows: list<list<string>>]: nothing -> string {
  let hdr = row-to-csv $header
  let csv_rows = $rows | each { |r| row-to-csv $r }
  [$hdr] | append $csv_rows | str join "\n"
}

export def extract-budget [source: string]: nothing -> string {
  let files = if ($source | path type) == "dir" {
    glob ($source | path join "*.md") | sort
  } else {
    [$source]
  }

  let all_tables = $files | each { |f|
    extract-budget-tables (open $f --raw)
  } | flatten

  if ($all_tables | is-empty) {
    error make { msg: "fin: no <!-- fin:budget:* --> markers found" }
  }

  let all_headers = $all_tables | each { |t| $t.header } | flatten | uniq
  let unified = $all_headers | append "kind"
  let kind_idx = ($unified | length) - 1

  let all_rows = $all_tables | each { |t|
    let col_map = $unified | each { |col|
      let found = $t.header | enumerate | where { |e| $e.item == $col }
      if ($found | is-not-empty) { $found | first | get index } else { -1 }
    }
    $t.rows | each { |row|
      mut cells = $col_map | each { |idx|
        if $idx >= 0 { $row | get $idx } else { "" }
      }
      $cells = ($cells | update $kind_idx $t.suffix)
      $cells
    }
  } | flatten

  table-to-csv $unified $all_rows
}

export def write-if-changed [content: string, path: string]: nothing -> bool {
  let existing = if ($path | path exists) { open $path --raw | str trim } else { "" }
  if $content != $existing {
    $content | save --force --raw $path
    true
  } else {
    false
  }
}

export def sync-md [md_source: string]: nothing -> bool {
  let dir = fin-dir
  let budget = extract-budget $md_source
  write-if-changed $budget ([$dir budget.csv] | path join)
}
