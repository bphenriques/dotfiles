local command=$1
shift

case "$command" in
  # Use `tree` to preview.
  cd)               fzf "$@" --preview 'tree -C {} | head -50' ;;

  # Use `tree` if it is a directory and `bat` otherwise.
  vi|vim|emacs|e)           fzf "$@" --preview '([ -d {-1} ] && tree -C {-1} | head -n 50) || bat --style=numbers --color=always {-1} 2> /dev/null' ;;

  # Use `echo` to show the value. 
  export|unset)     fzf "$@" --preview "eval 'echo \$'{}" ;;

  # Regular `fzf` otherwise.
  *)                fzf "$@" ;;
esac

