use std assert

use ../finlib/core.nu *
use ../finlib/budget.nu *
use ../finlib/ledger.nu [validate-source journal-path]
use ../finlib/markdown.nu [extract-budget write-if-changed sync-md]

# ── Validation ──

def "test validate-source passes" [] {
  let errors = validate-source
  assert equal $errors [] "validate-source should return no errors for valid fixtures"
}

# ── Budget loading ──

def "test load-budget" [] {
  let b = load-budget
  assert equal ($b | length) 16
  assert equal ($b | where kind == "income" | length) 1
  assert equal ($b | where kind == "expense" | length) 13
  assert equal ($b | where kind == "savings" | length) 2
  assert equal ($b | where when == "monthly" | length) 13
  assert equal ($b | where when == "yearly" | length) 2
  assert equal ($b | where when =~ '^\d{4}-' | length) 1
}

def "test split-names" [] {
  let names = split-names
  assert equal $names ["joint"]
}

def "test enrich-budget" [] {
  let enriched = load-budget | enrich-budget
  let splits = $enriched | where split_name != null
  assert (($splits | length) > 0) "should have split entries"
  for entry in $splits {
    assert ($entry.effective_amount > 0) "split effective_amount should be positive"
    assert ($entry.effective_amount <= $entry.row.amount) "effective_amount should not exceed original"
  }
}

# ── Account derivation ──

def "test category-to-account" [] {
  assert equal (category-to-account "Casa" "expense") "expenses:casa"
  assert equal (category-to-account "Casa > Seguro Casa" "expense") "expenses:casa:seguro-casa"
  assert equal (category-to-account "Salário" "income") "revenues:salario"
  assert equal (category-to-account "Fundo Emergência" "savings") "assets:savings:fundo-emergencia"
  assert equal (category-to-account "Casa > Eletricidade" "expense") "expenses:casa:eletricidade"
}

# ── Journal generation ──

def "test journal-generates" [] {
  let path = journal-path
  assert ($path | path exists) "journal file should exist"
  let content = open $path --raw
  assert ($content | str contains "commodity") "journal should have commodity directive"
  assert ($content | str contains "account expenses:casa") "journal should declare accounts"
  assert ($content | str contains "account expenses:casa:eletricidade") "journal should declare sub-accounts"
  assert ($content | str contains "account assets:savings:fundo-emergencia") "journal should declare savings accounts"
  assert ($content | str contains "~ monthly") "journal should have monthly rules"
  assert ($content | str contains "~ yearly") "journal should have yearly rules"
  assert ($content | str contains "2025-07-01") "journal should have one-off entry"
  assert ($content | str contains "Transfer joint") "journal should have split transfer"
  assert ($content | str contains "expenses:transportes") "journal should have personal-only expenses"
}

# ── Formatting ──

def "test fmt-currency" [] {
  let loc = { symbol: "€", decimal: ",", separator: ".", grouping: 3, p_cs_precedes: 0, p_sep_by_space: 1, n_cs_precedes: 0, n_sep_by_space: 1 }
  assert equal (fmt-currency 1234.56 $loc) "1.234,56 €"
  assert equal (fmt-currency 0.0 $loc) "0,00 €"
  assert equal (fmt-currency -50.0 $loc) "-50,00 €"
}

def "test amt rounding" [] {
  assert equal (1.006 | amt) 1.01
  assert equal (99.999 | amt) 100.0
}

# ── Slug ──

def "test slug-segment" [] {
  assert equal ("Eletricidade" | slug-segment) "eletricidade"
  assert equal ("Seguro Casa" | slug-segment) "seguro-casa"
  assert equal ("Subsídio Alimentação" | slug-segment) "subsidio-alimentacao"
  assert equal ("Luz + Gás" | slug-segment) "luz-gas"
}

# ── Markdown extraction ──

def "test extract-budget" [] {
  let fixture_dir = ([($env.FIN_DIR) .. fixtures] | path join | path expand)
  let csv = extract-budget $fixture_dir
  let expected = open ([($env.FIN_DIR) budget.csv] | path join) --raw | str trim
  assert equal $csv $expected "extracted budget should match fixture"
}

def "test write-if-changed" [] {
  let tmp = ([$env.XDG_RUNTIME_DIR fin-test] | path join)
  mkdir $tmp
  let path = [$tmp test.csv] | path join
  rm -f $path

  let wrote1 = write-if-changed "a,b\n1,2" $path
  assert $wrote1 "should write new file"

  let wrote2 = write-if-changed "a,b\n1,2" $path
  assert (not $wrote2) "should skip identical content"

  let wrote3 = write-if-changed "a,b\n3,4" $path
  assert $wrote3 "should overwrite changed content"

  rm -rf $tmp
}

def "test sync-md roundtrip" [] {
  let tmp = ([$env.XDG_RUNTIME_DIR fin-sync-test] | path join)
  mkdir $tmp
  let fixture_dir = ([($env.FIN_DIR) .. fixtures] | path join | path expand)

  let old_fin_dir = $env.FIN_DIR
  $env.FIN_DIR = $tmp
  let changed = sync-md $fixture_dir
  assert $changed "first sync should write files"

  let changed2 = sync-md $fixture_dir
  assert (not $changed2) "second sync should be no-op"

  let synced = open ([$tmp budget.csv] | path join) --raw | str trim
  let expected = open ([$old_fin_dir budget.csv] | path join) --raw | str trim
  assert equal $synced $expected "synced budget should match fixture"

  $env.FIN_DIR = $old_fin_dir
  rm -rf $tmp
}

# ── Runner ──

def main [] {
  test validate-source passes
  test load-budget
  test split-names
  test enrich-budget
  test category-to-account
  test journal-generates
  test fmt-currency
  test amt rounding
  test slug-segment
  test extract-budget
  test write-if-changed
  test sync-md roundtrip
  print "All tests passed ✓"
}
