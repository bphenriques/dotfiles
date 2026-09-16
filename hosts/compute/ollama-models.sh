# shellcheck shell=bash

json=(-H "Content-Type: application/json")

payload() { jq -nc --arg model "$1" '{model: $model}'; }

usage() {
  echo "usage: ollama-models list | pull <model> | rm <model>" >&2
  exit 1
}

list() {
  curl -sf "$OLLAMA_API/api/tags" \
    | jq -r '.models | sort_by(.name)[] | [.name, "\(.size/1073741824*10|round/10) GiB"] | @tsv' \
    | column -t -s $'\t'
}

pull() {
  local line status=""
  # /api/pull answers 200 even for a bad tag and reports failure inside the stream, so the last
  # line is the only verdict. Each chunk repeats its status, hence printing only what changed.
  while IFS= read -r line; do
    [ "$line" = "$status" ] || echo "$line"
    status=$line
  done < <(curl -sfN "${json[@]}" "$OLLAMA_API/api/pull" -d "$(payload "$1")" | jq -r --unbuffered '
    if (.total // 0) > 0
    then "\(.status) \((((.completed // 0) / .total) * 10 | floor) * 10)%"
    else .status // "error: \(.error)"
    end')
  [ "$status" = success ] || {
    echo "pull of $1 failed: ${status:-no response}" >&2
    exit 1
  }
}

remove() {
  curl -sf "${json[@]}" -X DELETE "$OLLAMA_API/api/delete" -d "$(payload "$1")" >/dev/null || {
    echo "failed to remove $1" >&2
    exit 1
  }
  echo "removed $1"
}

case "${1-}" in
  list)
    [ $# -eq 1 ] || usage
    list
    ;;
  pull)
    [ $# -eq 2 ] || usage
    pull "$2"
    ;;
  rm)
    [ $# -eq 2 ] || usage
    remove "$2"
    ;;
  *) usage ;;
esac
