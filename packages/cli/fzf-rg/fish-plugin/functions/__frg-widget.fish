function __frg-widget
    set --local buffer (builtin commandline --current-buffer | string trim -l)

    if test -z $buffer
        # fzf-rg emits file:line
        set --local target (fzf-rg)
        not test -z $target
        and test -f (string split --max 1 --fields 1 : -- $target)
        and $EDITOR $target
    else
        # insert just the file path, dropping :line
        set --local current_token (builtin commandline --current-token)
        set --local file (string split --max 1 --fields 1 : -- (fzf-rg $current_token))
        not test -z $file
        and builtin commandline -rt $file
        and builtin commandline --function repaint
    end
end
