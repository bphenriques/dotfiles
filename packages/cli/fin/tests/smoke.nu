use std assert

use ../finlib/core.nu *
use ../finlib/budget.nu *
use ../finlib/ledger.nu [validate-source derive-account journal-path]

# ── Validation ──

def "test validate-source passes" [] {
  let errors = validate-source
  assert equal $errors [] "validate-source should return no errors for valid fixtures"
}

# ── Budget loading ──

def "test load-categories" [] {
  let cats = load-categories
  assert equal ($cats | length) 4
  assert equal ($cats | where kind == "income" | length) 1
  assert equal ($cats | where kind == "expense" | length) 2
  assert equal ($cats | where kind == "savings" | length) 1
}

def "test load-budget" [] {
  let b = load-budget
  assert equal ($b | length) 6
  assert equal ($b | where cadence == "monthly" | length) 4
  assert equal ($b | where cadence == "yearly" | length) 1
  assert equal ($b | where cadence == "once" | length) 1
}

def "test split-names" [] {
  let names = split-names
  assert equal $names ["joint"]
}

def "test enrich-budget" [] {
  let cats = load-categories
  let enriched = load-budget | enrich-budget $cats
  let splits = $enriched | where split_name != null
  assert (($splits | length) > 0) "should have split entries"
  for entry in $splits {
    assert ($entry.effective_amount > 0) "split effective_amount should be positive"
    assert ($entry.effective_amount <= $entry.row.amount) "effective_amount should not exceed original"
  }
}

# ── Account derivation ──

def "test derive-account" [] {
  assert equal (derive-account "casa" "" "expense") "expenses:casa"
  assert equal (derive-account "casa" "seguro" "expense") "expenses:casa:seguro"
  assert equal (derive-account "salario" "" "income") "revenues:salario"
  assert equal (derive-account "investimentos" "" "savings") "assets:savings:investimentos"
}

# ── Journal generation ──

def "test journal-generates" [] {
  let path = journal-path
  assert ($path | path exists) "journal file should exist"
  let content = open $path --raw
  assert ($content | str contains "commodity") "journal should have commodity directive"
  assert ($content | str contains "account expenses:casa") "journal should declare accounts"
  assert ($content | str contains "~ monthly") "journal should have periodic rules"
  assert ($content | str contains "2025-07-01 Obras") "journal should have one-off entry"
  assert ($content | str contains "Transfer joint") "journal should have split transfer"
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

# ── Runner ──

def main [] {
  test validate-source passes
  test load-categories
  test load-budget
  test split-names
  test enrich-budget
  test derive-account
  test journal-generates
  test fmt-currency
  test amt rounding
  print "All tests passed ✓"
}
