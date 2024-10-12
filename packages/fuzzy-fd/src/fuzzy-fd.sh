#shellcheck shell=sh

#shellcheck disable=SC2016
__fish_shell() {
  echo '
function ffd
  set -l target (fuzzy-fd $argv[1])
  and if test -f $target
    $EDITOR $target
  else if test -d $target
    cd $target
  end
end
'
}

case "${1:-}" in
  "--init-shell")
    shift 1
    case "${1:-}" in
      fish) __fish_shell                     ;;
      *)    echo "Unsupported shell: $1"; exit 1  ;;
    esac
    ;;
  *)
    search="${1-}"
    fd 2>/dev/null | fzf --ansi --query "$search" --prompt="Find file: " --preview="preview {}"
    ;;
esac
