if [ ! "$#" -gt 0 ]; then
  echo "Usage: frg <text>";
  return 1;
fi

rg --hidden --files-with-matches --no-messages "$1" | fzf --preview-window '' --preview "rg --ignore-case --pretty --context 10 '$1' {}"
