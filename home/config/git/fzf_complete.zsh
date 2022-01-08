PREVIEW_DIRECTORY="([ -d {-1} ] && tree -C {-1} | head -n 50)"
PREVIEW_FILE="([ -f {-1} ] && bat --style=numbers --color=always {-1} 2>/dev/null)"
DIFF_FILE="(git add --intent-to-add {-1} && git diff --color=always {-1} 2>/dev/null)"

git_add() {
    git ls-files -m | _fzf_complete --multi --preview "$PREVIEW_DIRECTORY || $DIFF_FILE" -- "$@"
}

git_diff() {
    git ls-files -m | _fzf_complete --multi --preview "$PREVIEW_DIRECTORY || $DIFF_FILE" -- "$@"
}

git_rm() {
    git ls-files | _fzf_complete --multi --preview "$PREVIEW_DIRECTORY || $PREVIEW_FILE" -- "$@"
}

# TODO: Not perfect as it doesn't return the stash ids (_fzf_complete_git_post does not work for that). Fine for now :)
git_stash() {
    git stash list | _fzf_complete --preview 'echo {} | cut -d: -f1 | git stash show --color=always' -- "$@"
}

# TODO: Not perfect as it doesn't return the commit ids (_fzf_complete_git_post does not work for that). Fine for now :)
git_log() {
    preview="echo {} | grep --extended-regexp --only-matching --max-count 1 '[a-f0-9]{7,}' | head -1 | xargs -I% git show --color=always %"
    git log --graph --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr%Creset' | _fzf_complete --ansi --print0 --preview="$preview" -- "$@"
}

# TODO: Glob pattern matching is not perfect as it may clash (e.g., log and reflog).
_fzf_complete_git() {
    case "$@" in
         *add*)     git_add "$@"    ;;
         *diff*)    git_diff "$@"   ;;
         *rm*)      git_rm  "$@"    ;;
         *stash*)   git_stash "$@"  ;;
         *log*)     git_log "$@"    ;;
         *)                         ;;
    esac
}
