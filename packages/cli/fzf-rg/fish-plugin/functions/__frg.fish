function __frg
    # fzf-rg emits file:line
    set --local target (fzf-rg $argv[1])
    not test -z $target
    and test -f (string split --max 1 --fields 1 : -- $target)
    and $EDITOR $target
end
