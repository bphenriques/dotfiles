# shellcheck shell=bash

json=(-H "Content-Type: application/json")

deadline=$((SECONDS + 300))
until curl -sf "$OLLAMA_API/api/version" >/dev/null; do
  [ "$SECONDS" -lt "$deadline" ] || {
    echo "ollama did not answer within 300s" >&2
    exit 1
  }
  sleep 2
done

read -ra models <<<"$DECLARED_MODELS"
# An empty declared set would prune every model this unit pulled: a mistake rather than an intent.
[ "${#models[@]}" -gt 0 ] || {
  echo "no models declared" >&2
  exit 1
}

for model in "${models[@]}"; do
  echo "pulling $model"
  # /api/pull answers 200 even for a bad tag and reports failure inside the stream, so the last
  # line is the only verdict. A failed request yields none, which is why success is matched.
  if ! verdict=$(curl -sf "${json[@]}" "$OLLAMA_API/api/pull" -d "{\"model\":\"$model\"}" | tail -1); then
    echo "pull of $model failed: request error" >&2
    exit 1
  fi
  case "$verdict" in
    *'"status":"success"'*) ;;
    *)
      echo "pull of $model failed: ${verdict:-no response}" >&2
      exit 1
      ;;
  esac
done

# /api/tags reports a tag on every model, so untagged declarations are recorded as ":latest".
qualify() { case "$1" in *:*) echo "$1" ;; *) echo "$1:latest" ;; esac }

declared=()
for model in "${models[@]}"; do declared+=("$(qualify "$model")"); done

# Pruning reads what this unit pulled last time rather than /api/tags: a model pulled by hand is not
# ours to delete.
managed="$STATE_DIRECTORY/managed"
if [ -f "$managed" ]; then
  while read -r model; do
    case " ${declared[*]} " in
      *" $model "*) continue ;;
    esac
    echo "removing $model, no longer declared"
    curl -sf "${json[@]}" -X DELETE "$OLLAMA_API/api/delete" -d "{\"model\":\"$model\"}" >/dev/null \
      || echo "failed to remove $model" >&2
  done <"$managed"
fi

printf '%s\n' "${declared[@]}" >"$managed"

# Ollama has no per-model keep_alive setting: the value sticks to the loaded instance and later
# requests preserve it, so pinning once here outlives the finite default the other models get.
echo "pinning $PINNED_MODEL"
curl -sf "${json[@]}" "$OLLAMA_API/api/generate" \
  -d "{\"model\":\"$PINNED_MODEL\",\"prompt\":\"\",\"stream\":false,\"keep_alive\":-1}" >/dev/null \
  || echo "failed to pin $PINNED_MODEL" >&2
