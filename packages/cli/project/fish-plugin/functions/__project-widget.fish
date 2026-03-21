function __project-widget
  set --local buffer (commandline --current-buffer)
  set --local target (project --select $buffer)

  # Ensure prompt does not get broken if I cancel
  builtin commandline --function cancel-commandline repaint

  not test -z $target
  and test -d $target
  and cd $target
end