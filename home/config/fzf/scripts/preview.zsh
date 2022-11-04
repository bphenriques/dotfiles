__preview() {
  mime=$(file -bL --mime-type "$1")
  category=${mime%%/*}
  kind=${mime##*/}

  if [ -d "$1" ]; then
    tree -C "$1"
  elif [ -f "$1" ]; then
    case "$category" in
        image)        chafa "$1" ;;
        text)         bat --style=numbers --color=always "$1" 2>/dev/null ;;
        application)
                      case "$kind" in
                        json)   jq --color-output '.' "$1" ;;
                        *)      bat --style=numbers --color=always "$1" 2>/dev/null ;;
                      esac
                      ;;
        *)            bat --color=always "$1" 2>/dev/null ;;
     esac
  fi
}

if [ $# -eq 1 ]; then
  __preview "$1" | head -200
fi



