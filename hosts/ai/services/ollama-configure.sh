# shellcheck shell=bash

json=(-H "Content-Type: application/json")

payload() { jq -nc --arg model "$1" '{model: $model}'; }

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

# A failed pull (say, no internet at boot) must not skip the pin below, so it fails the unit at the end.
failed=0
create=()
for model in "${models[@]}"; do
  if jq -e --arg m "$model" 'has($m)' "$BUILT_MODELS" >/dev/null; then
    create+=("$model")
    continue
  fi
  echo "pulling $model"
  # /api/pull answers 200 even for a bad tag and reports failure inside the stream, so the last
  # line is the only verdict. A failed request yields none, which is why success is matched.
  if ! verdict=$(curl -sf "${json[@]}" "$OLLAMA_API/api/pull" -d "$(payload "$model")" | tail -1); then
    echo "pull of $model failed: request error" >&2
    failed=1
    continue
  fi
  case "$verdict" in
    *'"status":"success"'*) ;;
    *)
      echo "pull of $model failed: ${verdict:-no response}" >&2
      failed=1
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
    curl -sf "${json[@]}" -X DELETE "$OLLAMA_API/api/delete" -d "$(payload "$model")" >/dev/null \
      || echo "failed to remove $model" >&2
  done <"$managed"
fi

printf '%s\n' "${declared[@]}" >"$managed"

# Ollama has no per-model keep_alive setting: the value sticks to the loaded instance and later
# requests preserve it, so pinning once here outlives the finite default the other models get.
echo "pinning $PINNED_MODEL"
curl -sf "${json[@]}" "$OLLAMA_API/api/generate" \
  -d "$(jq -nc --arg model "$PINNED_MODEL" '{model: $model, prompt: "", stream: false, keep_alive: -1}')" >/dev/null \
  || echo "failed to pin $PINNED_MODEL" >&2

# Created only when missing: a Modelfile edit takes effect after `ollama rm`, since each create downloads ~100 GB.
build() {
  local model=$1 dir="$CACHE_DIRECTORY/$1"
  mkdir -p "$dir"
  while read -r url; do
    curl -sfL --retry 5 -C - -o "$dir/$(basename "$url")" "$url" || return 1
  done < <(jq -r --arg m "$model" '.[$m].files[]' "$BUILT_MODELS")
  cp "$(jq -r --arg m "$model" '.[$m].modelfile' "$BUILT_MODELS")" "$dir/Modelfile"
  podman run --rm --network=host -v "$dir:/models:ro" -w /models -e OLLAMA_HOST="$OLLAMA_API" \
    --entrypoint ollama "$OLLAMA_IMAGE" create "$model" -f Modelfile || return 1
  rm -rf "$dir"
}

present=$(curl -sf "$OLLAMA_API/api/tags" | jq -r '.models[].name') || {
  echo "could not list models" >&2
  exit 1
}
for model in "${create[@]}"; do
  grep -qxF "$(qualify "$model")" <<<"$present" && continue
  echo "creating $model"
  build "$model" || {
    echo "create of $model failed" >&2
    failed=1
  }
done

exit "$failed"
