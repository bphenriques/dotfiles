#shellcheck shell=bash

PREVIEW_WIDTH_CHARS=150
exists_cmd() { command -v "$1" >/dev/null; }
open_dmenu() {
  if exists_cmd fuzzel; then
    fuzzel --dmenu --prompt 'Paste ' --width "${PREVIEW_WIDTH_CHARS}"
  else
    echo "No compatible dmenu runner found" >&2;
    exit 1
  fi
}

cliphist -preview-width "${PREVIEW_WIDTH_CHARS}" list | open_dmenu | cliphist decode | wl-copy
