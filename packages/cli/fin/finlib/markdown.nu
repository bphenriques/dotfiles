def normalize-decimal []: string -> string {
  let v = $in
  if ($v =~ '^-?\d+,\d{1,2}$') { $v | str replace ',' '.' } else { $v }
}

def parse-md-row []: string -> list<string> {
  str trim
    | str replace --regex '^\|' ''
    | str replace --regex '\|$' ''
    | split row '|'
    | each { str trim | str replace --all '**' '' | normalize-decimal }
}

def parse-marker-suffix [raw: string]: nothing -> record<kind: string, defaults: record> {
  let parts = $raw | split row ' ' | where { |p| $p != '' }
  let defaults = $parts | skip 1 | reduce -f {} { |p, acc|
    let kv = $p | split row '=' | each { str trim }
    if ($kv | length) == 2 { $acc | insert $kv.0 $kv.1 } else { $acc }
  }
  { kind: ($parts | first), defaults: $defaults }
}

def extract-budget-tables [file: string]: nothing -> list {
  let content = open $file --raw
  let lines = $content | lines
  let marker_re = "<!-- fin:budget:"
  let markers = $lines | enumerate | where { |e| ($e.item | str trim | str downcase | str starts-with $marker_re) }

  $markers | each { |m|
    let suffix = $m.item | str trim | str replace --regex '(?i)<!-- fin:budget:' "" | str replace " -->" "" | str trim
    let parsed = parse-marker-suffix $suffix
    let after = $lines | enumerate | skip ($m.index + 1)
      | skip while { |e| ($e.item | str trim) == "" }
    let table_lines = $after | take while { |e| ($e.item | str trim | str starts-with "|") }
    if ($table_lines | length) < 2 {
      let loc = $"($file | path basename):($m.index + 1)"
      error make { msg: $"fin: marker at ($loc) has no table following it" }
    }
    let header = $table_lines | first | get item | parse-md-row | each { str downcase | str trim | str replace --all ' ' '-' }
    let expected = $header | length
    let data = $table_lines | skip 2 | each { |e|
      let cells = ($e.item | parse-md-row)
      if ($cells | length) != $expected {
        let loc = $"($file | path basename):($e.index + 1)"
        error make { msg: $"fin: at ($loc): row has ($cells | length) cells, header has ($expected)" }
      }
      { cells: $cells, line: ($e.index + 1) }
    }
    { header: $header, rows: $data, kind: $parsed.kind, defaults: $parsed.defaults, file: $file }
  } | compact | where { |t| ($t.rows | is-not-empty) }
}

export def extract-budget [source: string]: nothing -> table {
  let files = if ($source | path type) == "dir" {
    glob ($source | path join "*.md") | sort
  } else {
    [$source]
  }

  let all_tables = $files | each { |f| extract-budget-tables $f } | flatten

  if ($all_tables | is-empty) {
    error make { msg: "fin: no <!-- fin:budget:* --> markers found" }
  }

  let table_headers = $all_tables | each { |t| $t.header } | flatten | uniq
  let default_keys = $all_tables | each { |t| $t.defaults | columns } | flatten | uniq
  let all_headers = $table_headers | append $default_keys | uniq

  $all_tables | each { |t|
    let header_idx = $t.header | enumerate | reduce -f {} { |e, acc| $acc | insert $e.item $e.index }
    $t.rows | each { |entry|
      let rec = $all_headers | reduce -f {} { |col, acc|
        let val = if ($col in $header_idx) { $entry.cells | get ($header_idx | get $col) } else { "" }
        let final_val = if $val == "" and ($col in $t.defaults) { $t.defaults | get $col } else { $val }
        $acc | insert $col $final_val
      }
      $rec | insert kind $t.kind
    }
  } | flatten
}
