#!/usr/bin/env bash

# Stop hook: prevents Claude from finishing with uncommitted or unpushed work.
# Exit 0 = allow stop, Exit 2 = block stop (message on stderr).

input=$(cat)

# Prevent recursion
stop_hook_active=$(echo "$input" | jq -r '.stop_hook_active')
if [[ "$stop_hook_active" = "true" ]]; then
  exit 0
fi

# Only run in git repositories
if ! git rev-parse --git-dir >/dev/null 2>&1; then
  exit 0
fi

# Check for uncommitted changes (staged or unstaged)
if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "There are uncommitted changes. Please commit and push before finishing." >&2
  exit 2
fi

# Check for untracked files
if [[ -n "$(git ls-files --others --exclude-standard)" ]]; then
  echo "There are untracked files. Please commit and push before finishing." >&2
  exit 2
fi

# Check for unpushed commits
current_branch=$(git branch --show-current)
if [[ -n "$current_branch" ]]; then
  if git rev-parse "origin/$current_branch" >/dev/null 2>&1; then
    unpushed=$(git rev-list "origin/$current_branch..HEAD" --count 2>/dev/null) || unpushed=0
  else
    unpushed=$(git rev-list "origin/HEAD..HEAD" --count 2>/dev/null) || unpushed=0
  fi

  if [[ "$unpushed" -gt 0 ]]; then
    echo "There are $unpushed unpushed commit(s) on '$current_branch'. Please push before finishing." >&2
    exit 2
  fi
fi

exit 0
