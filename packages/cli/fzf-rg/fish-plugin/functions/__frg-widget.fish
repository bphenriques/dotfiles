function __frg-widget
  set --local buffer (builtin commandline --current-buffer | string trim -l)

  if test -z $buffer
    set --local target (fzf-rg)
    not test -z $target
    and test -f $target
    and $EDITOR $target
  else
    # if buffer is not empty, replace the current token with the search result
    set --local current_token (builtin commandline --current-token)
    set --local target (fzf-rg $current_token)
    builtin commandline -rt $target
    and builtin commandline --function repaint
  end
end