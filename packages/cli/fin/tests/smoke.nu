use std assert
use ../finlib/core.nu [fmt-currency amt]
use ../finlib/budget.nu [validate-budget load-budget enrich-budget split-monthly-transfer]
use ../finlib/reports.nu [dashboard-data]

def "test validates" [] {
  assert equal (validate-budget) [] "fixtures should validate"
}

def "test budget shape" [] {
  let b = load-budget
  assert equal ($b | length) 17
  # marker default: income table omits Frequency column
  assert equal ($b | where kind == "income" | first | get frequency) "monthly"
}

# Monthly: 200+300+25+17.5+12.5+7.5 = 562.50
# Yearly/12: (90+250)/12 = 28.33
# Quarterly/3: 45/3 = 15
# Sum: 605.83
def "test transfer math" [] {
  let total = load-budget | enrich-budget | split-monthly-transfer | get total | math sum
  assert ($total > 605 and $total < 607)
}

def "test joint-equals-transfer" [] {
  let data = dashboard-data "month"
  let transfer = load-budget | enrich-budget | split-monthly-transfer | get total | math sum
  assert equal $data.joint_expense $transfer
}

def "test formatting" [] {
  assert equal (fmt-currency 1234.56) "1.234,56 €"
  assert equal (fmt-currency 0.0) "0,00 €"
  assert equal (fmt-currency -50.0) "-50,00 €"
  assert equal (1.006 | amt) 1.01
  assert equal (99.999 | amt) 100.0
}

def main [] {
  test validates
  test budget shape
  test transfer math
  test joint-equals-transfer
  test formatting
  print "All tests passed ✓"
}
