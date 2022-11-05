__FZF_CUSTOM_GIT_PREVIEW_DIRECTORY="([ -d {-1} ] && tree -C {-1} | head -n 50)"
__FZF_CUSTOM_GIT_DIFF_FILE="(git add --intent-to-add {-1} && git diff --color=always {-1} 2>/dev/null)"

__fzf_git_add() {
  git ls-files -m | _fzf_complete --multi --preview "$__FZF_CUSTOM_GIT_PREVIEW_DIRECTORY || $__FZF_CUSTOM_GIT_DIFF_FILE" -- "$@"
}

__fzf_git_diff() {
  git ls-files -m | _fzf_complete --multi --preview "$__FZF_CUSTOM_GIT_PREVIEW_DIRECTORY || $__FZF_CUSTOM_GIT_DIFF_FILE" -- "$@"
}

# TODO: Not perfect as it doesn't return the stash ids (_fzf_complete_git_post does not work for that). Fine for now :)
__fzf_git_stash() {
  git stash list | _fzf_complete --preview 'echo {} | cut -d: -f1 | xargs -I% git stash show --color=always %' -- "$@"
}

# TODO: Not perfect as it doesn't return the commit ids (_fzf_complete_git_post does not work for that). Fine for now :)
__fzf_git_log() {
  preview="echo {} | grep --extended-regexp --only-matching --max-count 1 '[a-f0-9]{7,}' | head -1 | xargs -I% git show --color=always %"
  git log --graph --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr%Creset' | _fzf_complete --ansi --print0 --preview="$preview" -- "$@"
}

# TODO: Glob pattern matching is not perfect as it may clash (e.g., log and reflog).
_fzf_complete_git() {
  case "$@" in
    *add*)    __fzf_git_add "$@" ;;
    *diff*)   __fzf_git_diff "$@" ;;
    *rm*)     __fzf_git_rm "$@" ;;
    *stash*)  __fzf_git_stash "$@" ;;
    *log*)    __fzf_git_log "$@" ;;
    *) ;;
  esac
}
