# shellcheck shell=bash

deadline=$((SECONDS + 300))
until curl -sf "$OLLAMA_API/api/version" >/dev/null; do
  [ "$SECONDS" -lt "$deadline" ] || {
    echo "ollama did not answer within 300s" >&2
    exit 1
  }
  sleep 2
done

read -ra models <<<"$OLLAMA_MODELS"
for model in "${models[@]}"; do
  echo "pulling $model"
  # /api/pull answers 200 even for a bad tag and reports failure inside the stream, so the last
  # line is the only verdict. A failed request yields none, which is why success is matched.
  if ! verdict=$(curl -sf "$OLLAMA_API/api/pull" -d "{\"model\":\"$model\"}" | tail -1); then
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
