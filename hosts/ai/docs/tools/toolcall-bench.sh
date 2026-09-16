#!/usr/bin/env bash
# Tool-call reliability probe for the ai host, against /v1 (the path hermes uses).
#
#   ./toolcall-bench.sh                                   # defaults, 18-tool baseline
#   ./toolcall-bench.sh -n 15 -t tools-baseline.json      # more trials
#   ./toolcall-bench.sh -c 40000                          # inflate context with a tool result
#
# Scoring is per scenario: the right tool AND the right argument, or for the negative
# scenario no tool at all. A wrong argument counts as a failure, because at depth that is
# the dominant failure mode and it produces confident wrong actions.
set -euo pipefail

MODEL=qwen3.6:35b-a3b
ENDPOINT=http://ai:11434/v1
TRIALS=5
CHARS=0
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TOOLS="$HERE/tools-baseline.json"

while getopts "m:n:t:c:e:h" o; do
  case $o in
    m) MODEL=$OPTARG ;; n) TRIALS=$OPTARG ;; t) TOOLS=$OPTARG ;;
    c) CHARS=$OPTARG ;; e) ENDPOINT=$OPTARG ;;
    h)
      sed -n '2,9p' "${BASH_SOURCE[0]}"
      exit 0
      ;;
    *) exit 2 ;;
  esac
done

command -v jq >/dev/null || {
  echo "jq required" >&2
  exit 1
}
SOUL="$(cat "$HERE/../../../guests/agent-vm/SOUL.md")"
T="$(cat "$TOOLS")"

# Filler stands in for a prior tool result. Repo text, so it carries plausible paths the
# model can wrongly lift into arguments; synthetic filler understates that failure.
filler() {
  [ "$CHARS" -gt 0 ] || return 0
  set +o pipefail # `head -c` closes the pipe early, SIGPIPEing cat; that is expected here
  find "$HERE/../../../.." -path '*/.git' -prune -o \( -name '*.nix' -o -name '*.md' \) -print 2>/dev/null \
    | head -120 | xargs cat 2>/dev/null | head -c "$CHARS"
}
FILL="$(filler)"

history_for() {
  [ "$CHARS" -gt 0 ] || {
    echo '[]'
    return
  }
  jq -nc --arg r "$FILL" '[
    {role:"user",content:"Fetch https://example.invalid/spec and summarise it."},
    {role:"assistant",content:null,tool_calls:[{id:"call_1",type:"function",
      function:{name:"fetch",arguments:"{\"url\":\"https://example.invalid/spec\"}"}}]},
    {role:"tool",tool_call_id:"call_1",content:$r},
    {role:"assistant",content:"That document covers NixOS module and derivation structure."}]'
}
HIST="$(history_for)"

pass=0
total=0
detail=""
for _ in $(seq 1 "$TRIALS"); do
  for sc in time vault negative; do
    case $sc in
      time) q="What time is it in Tokyo right now?" ;;
      vault) q="Read my note at projects/homelab.md" ;;
      negative) q="In one sentence, explain what a symlink is. Do not use any tools." ;;
    esac
    body=$(jq -nc --argjson t "$T" --arg m "$MODEL" --arg s "$SOUL" --arg q "$q" --argjson h "$HIST" \
      '{model:$m,messages:([{role:"system",content:$s}]+$h+[{role:"user",content:$q}]),tools:$t,stream:false}')
    r=$(curl -sS -m 900 "$ENDPOINT/chat/completions" -H 'Content-Type: application/json' -d "$body")
    pt=$(jq -r '.usage.prompt_tokens // "?"' <<<"$r")
    nm=$(jq -r '.choices[0].message.tool_calls[0].function.name // ""' <<<"$r")
    ag=$(jq -r '.choices[0].message.tool_calls[0].function.arguments // ""' <<<"$r")
    total=$((total + 1))
    case $sc in
      time) { [ "$nm" = get_current_time ] && jq -r '.timezone' <<<"$ag" 2>/dev/null | grep -qi tokyo; } \
        && pass=$((pass + 1)) || detail="$detail t:${nm:-none}" ;;
      vault) { [ "$nm" = read_note ] && jq -r '.path' <<<"$ag" 2>/dev/null | grep -q homelab; } \
        && pass=$((pass + 1)) || detail="$detail v:${nm:-none}" ;;
      negative) [ -z "$nm" ] && pass=$((pass + 1)) || detail="$detail n:spurious($nm)" ;;
    esac
  done
done

printf '%-24s %2s tools  prompt=%-6s %3d/%-3d %s\n' \
  "$MODEL" "$(jq length "$TOOLS")" "$pt" "$pass" "$total" "$detail"
