# shellcheck shell=bash

FINANCE_DIR="$(dirname "$LEDGER_FILE")"
RECALIBRATE_FILE="${FINANCE_DIR}/recalibrate.journal"

require_journal() {
  if [ ! -r "$LEDGER_FILE" ]; then
    printf 'fin: journal not readable: %s\n' "$LEDGER_FILE" >&2
    printf 'Is the NAS mounted?\n' >&2
    exit 2
  fi
}

usage() {
  cat <<EOF
fin <command> [args]

  Commands:
    [y]ear               Annual totals by category
    [m]onth              This month's detail by category
    [s]hared             Joint account: how much to transfer this month
    sa[v]ings            Savings breakdown
    [b]udget             Budget vs actuals (recalibration months)
    [d]ashboard          Visual summary dashboard
    [c]heck              Validate journal (accounts, balances)
    [e]dit               Open plan.journal in \$EDITOR
    [u]i                 Interactive TUI (hledger-ui)
    [w]eb                Web UI (hledger-web)
    -- [args]            Pass arguments directly to hledger

  Examples:
    fin d                                         visual summary
    fin -- balance -M --depth 3 expenses:casa     ad-hoc exploration
    fin -- register expenses:supermercado         transaction list
EOF
}

require_journal

case "${1:-}" in
  year | y)
    hledger balance -Y -E --depth 2 --forecast='this year' -p 'this year' revenues expenses assets:savings
    ;;
  month | m)
    hledger balance -E --depth 2 --forecast='this month' -p 'this month' revenues expenses assets:savings
    ;;
  shared | s)
    hledger balance -p 'this month' --forecast='this month' desc:partilhados not:assets:banco
    ;;
  savings | v)
    hledger balance -Y -E --depth 3 --forecast='this year' -p 'this year' assets:savings
    ;;
  budget | b)
    hledger -f "$RECALIBRATE_FILE" balance --budget -M -E -p 'this year'
    ;;
  dashboard | d)
    nu --no-config-file "$FIN_DASHBOARD"
    ;;
  check | c)
    hledger check -s --forecast='this year'
    echo "OK"
    ;;
  edit | e)
    ${EDITOR:-vi} "${FINANCE_DIR}/plan.journal"
    ;;
  ui | u)
    hledger-ui --forecast='this year'
    ;;
  web | w)
    shift
    hledger-web --serve --forecast='this year' "$@"
    ;;
  --)
    shift
    hledger "$@"
    ;;
  *)
    usage
    exit 1
    ;;
esac
