__preview_text() {
  bat --style=numbers --color=always "$1" 2>/dev/null
}

__preview_json() {
  jq --color-output '.' "$1"
}

__preview_image() {
  chafa "$1"
}

__preview() {
  mime=$(file -bL --mime-type "$1")
  category=${mime%%/*}
  kind=${mime##*/}

  if [ -d "$1" ]; then
    tree -C "$1"
  elif [ -f "$1" ]; then
    case "$category" in
      text)           __preview_text "$1" ;;
      application)
        case "$kind" in
          json)       __preview_json "$1" ;;
          *)          __preview_text "$1" ;;
        esac
        ;;
      image)          __preview_image "$1" ;;
      *)              bat --color=always "$1" 2>/dev/null ;;
    esac
  fi
}

if [ $# -eq 1 ]; then
  __preview "$1" | head -200
fi
