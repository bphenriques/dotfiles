__preview_default() {
  bat --color=always "$1" 2>/dev/null | head -200;
}

__preview_text() {
  bat --style=numbers --color=always "$1" 2>/dev/null | head -200;
}

__preview_yaml() {
  yq --color-output '.' "$1" | head -200;
}

__preview_zip() {
  zipinfo -1 "$1" | head -200;
}

__preview_pdf() {
  # TODO: Use imagemagick and ghostscript to convert the first image to png
  __preview_text "$1"
}

__preview_json() {
  jq --color-output '.' "$1" | head -200;
}

__preview_image() {
  # The only format that works reliably is symbols
  chafa --colors full --format symbols --work 1 "$1"
}

__preview() {
  mime=$(file -bL --mime-type "$1")
  category=${mime%%/*}
  kind=${mime##*/}

  if [ -d "$1" ]; then
    tree -C "$1"
  elif [ -f "$1" ]; then
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
