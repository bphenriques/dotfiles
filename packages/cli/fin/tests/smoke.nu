use std assert

use ../finlib/core.nu *
use ../finlib/budget.nu *
use ../finlib/ledger.nu [journal-path]
use ../finlib/markdown.nu [extract-budget]

# ── Validation ──

def "test validation passes" [] {
  let errors = validate-budget
  assert equal $errors [] "validate-budget should return no errors for valid fixtures"
}

# ── Budget loading ──

def "test load-budget" [] {
  let b = load-budget
  assert equal ($b | length) 18
  assert equal ($b | where kind == "income" | length) 1
  assert equal ($b | where kind == "expense" | length) 15
  assert equal ($b | where kind == "savings" | length) 2
  assert equal ($b | where frequency == "monthly" | length) 13
  assert equal ($b | where frequency == "yearly" | length) 3
  assert equal ($b | where frequency == "quarterly" | length) 1
  assert equal ($b | where frequency =~ '^\d{4}-' | length) 1
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

# ── Split transfers ──

def "test split-monthly-transfer" [] {
  let transfers = split-monthly-transfer
  assert equal ($transfers | length) 1 "should have one transfer entry (joint)"
  let joint = $transfers | first
  assert equal $joint.split_name "joint"

  # Monthly split: 200+300+25+17.5+12.5+7.5 = 562.5
  # Yearly split: (90+250)/12 = 28.33
  # Quarterly split: 45/3 = 15
  # One-off excluded from normalization
  # Total: 562.5 + 28.33 + 15 = 605.83
  assert ($joint.total > 605) "monthly transfer should include normalized yearly+quarterly"
  assert ($joint.total < 607) "monthly transfer should be around 605.83"
}

# ── Slugs and accounts ──

def "test slugs-and-accounts" [] {
  assert equal ("Eletricidade" | slug-segment) "eletricidade"
  assert equal ("Seguro Casa" | slug-segment) "seguro-casa"
  assert equal ("Subsídio Alimentação" | slug-segment) "subsidio-alimentacao"
  assert equal ("Luz + Gás" | slug-segment) "luz-gas"
  assert equal ("ETF MSCI World" | slug-segment) "etf-msci-world"
  assert equal (category-to-account "Casa" "expense") "expenses:casa"
  assert equal (category-to-account "Casa > Seguro Casa" "expense") "expenses:casa:seguro-casa"
  assert equal (category-to-account "Salário" "income") "revenues:salario"
  assert equal (category-to-account "Fundo Emergência" "savings") "assets:savings:fundo-emergencia"
}

# ── Journal generation ──

def "test journal-structure" [] {
  let content = open (journal-path) --raw
  assert ($content | str contains "commodity") "journal should have commodity directive"
  assert ($content | str contains "account expenses:casa") "should declare accounts"
  assert ($content | str contains "account expenses:casa:eletricidade") "should declare sub-accounts"
  assert ($content | str contains "account assets:savings:fundo-emergencia") "should declare savings accounts"
  assert ($content | str contains "~ monthly") "should have monthly rules"
  assert ($content | str contains "~ yearly") "should have yearly rules"
  assert ($content | str contains "~ every 3 months") "should have quarterly rules"
  assert ($content | str contains "2025-12-01 Natal") "should have dated one-off entry"
  assert ($content | str contains "~ monthly  Transfer joint") "should have monthly split transfer"
  assert ($content | str contains "expenses:transportes") "should have personal-only expenses"
}

def "test journal-tags" [] {
  let content = open (journal-path) --raw
  assert ($content | str contains "source:budget") "should have source:budget tag"
  assert ($content | str contains "kind:expense") "should have kind tag"
  assert ($content | str contains "cadence:monthly") "should have cadence tag"
  assert ($content | str contains "split:joint") "should have split tag"
  assert ($content | str contains "share:0.5") "should have share tag"
  assert ($content | str contains "cadence:2025-12") "should have one-off cadence tag"
  assert ($content | str contains "Transfer joint ; source:budget") "transfer should have source tag"
  assert ($content | str contains "ref:casa") "wiki link should produce ref tag"
  assert (not ($content | str contains "[[Casa]]")) "narration should not contain raw wiki link"
}

# ── Formatting ──

def "test formatting" [] {
  assert equal (fmt-currency 1234.56) "1.234,56 €"
  assert equal (fmt-currency 0.0) "0,00 €"
  assert equal (fmt-currency -50.0) "-50,00 €"
  assert equal (1.006 | amt) 1.01
  assert equal (99.999 | amt) 100.0
}

# ── Markdown extraction ──

def "test extract-budget" [] {
  let rows = extract-budget ($env.FIN_DIR | path expand)
  assert equal ($rows | length) 18
  assert equal ($rows | where kind == "income" | length) 1
  assert ("split-joint" in ($rows | columns)) "should have split-joint column"
  # Marker defaults
  assert equal ($rows | where kind == "income" | first | get frequency) "monthly" "income frequency from marker default"
  assert equal ($rows | where frequency == "quarterly" | length) 1 "explicit quarterly overrides default"
  assert (($rows | where frequency == "yearly" | length) > 0) "explicit yearly overrides default"
}

# ── Dashboard ──

def "test split-footer-matches-transfer" [] {
  use ../finlib/reports.nu [dashboard-data]
  let data = dashboard-data "month"
  let transfer = split-monthly-transfer | get total | append 0 | math sum
  assert equal $data.split_my_total $transfer "split footer my_total should match normalized transfer"
}

# ── Runner ──

def main [] {
  test validation passes
  test load-budget
  test enrich-budget
  test split-monthly-transfer
  test slugs-and-accounts
  test journal-structure
  test journal-tags
  test formatting
  test extract-budget
  test split-footer-matches-transfer
  print "All tests passed ✓"
}
