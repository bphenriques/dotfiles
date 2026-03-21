function __project
  if test (count $argv) -eq 0
    cd $PROJ_ROOT
  else
    set --local target (project --select $argv[1])

    # Ensure prompt does not get broken if I cancel
    builtin commandline --function cancel-commandline repaint

    not test -z $target
    and test -d $target
    and cd $target
  end
end