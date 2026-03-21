function __ffd-widget
  set --local buffer (builtin commandline --current-buffer | string trim -l)

  if test -z $buffer
    set --local target (fzf-fd)

    not test -z $target
    and if test -f $target
      $EDITOR $target
    else if test -d $target
      # Ensure prompt does not get broken if I cancel
      builtin commandline --function cancel-commandline repaint
      cd $target
    end
  else
    # if buffer is not empty, replace the current token with the search result
    set --local current_token (builtin commandline --current-token)
    set --local target (fzf-fd $current_token)
    builtin commandline -rt $target
    and builtin commandline --function repaint
  end
end