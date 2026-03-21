function __ffd
  set --local target (fzf-fd $argv[1])

  not -z $target
  and if test -f $target
    $EDITOR $target
  else if test -d $target
    # Ensure prompt does not get broken if I cancel
    builtin commandline --function cancel-commandline repaint
    cd $target
  end
end