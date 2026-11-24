# The id goes into a command the guest's shell reads, so only the characters Hermes uses for one pass.
session=${1:-}
if [[ ! $session =~ ^[A-Za-z0-9_-]*$ ]]; then
  echo "Not a session: $session" >&2
  exit 1
fi
exec ssh -t "$SSH_HOST" alfredo tui "$session"
