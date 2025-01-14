__preview_default() { bat --color=always "$1" 2>/dev/null | head -200; }
__preview_text() { bat --style=numbers --color=always "$1" 2>/dev/null | head -200; }
__preview_yaml() { yq --color-output '.' "$1" | head -200; }
__preview_zip() { zipinfo -1 "$1" | head -200; }
__preview_pdf() { __preview_text "$1"; }
__preview_json() { jq --color-output '.' "$1" | head -200; }
__preview_dir() { tree -L 2 -C "$1" | head -200; }

# The only format that works reliably is symbols
__preview_image() { chafa --colors full --format symbols --work 1 "$1"; }


__preview() {
  if [ -d "$1" ]; then
    __preview_dir "$1"
  elif [ -f "$1" ]; then
    mime=$(file -bL --mime-type "$1")
    category=${mime%%/*}
    kind=${mime##*/}
    case "$category" in
      text)
        if [[ "$1" =~ \.[Yy][Aa][Mm][Ll]$ ]]; then
          __preview_yaml "$1"
        else
          __preview_text "$1"
        fi
        ;;
      application)
        case "$kind" in
          json)       __preview_json "$1" ;;
          pdf)        __preview_pdf "$1" ;;
          zip)        __preview_zip "$1" ;;
          *)          __preview_text "$1" ;;
        esac
        ;;
      image)          __preview_image "$1" ;;
      *)              __preview_default "$1" ;;
    esac
  fi
}

if [ $# -eq 1 ]; then
  __preview "$1"
fi
