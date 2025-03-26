function __frg
  set --local target (fzf-rg $argv[1])
  not test -z $target
  and test -f $target
  and $EDITOR $target
end