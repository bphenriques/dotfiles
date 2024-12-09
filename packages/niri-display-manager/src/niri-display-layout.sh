#shellcheck shell=sh
list() {
  echo "list"
}

set() {
  echo ""
}

case "${1:-}" in
  list) shift 1 && delta "+${1:-10}%" && notify   ;;
  set)  shift 1 && delta "${1:-10}-%" && notify   ;;
esac
