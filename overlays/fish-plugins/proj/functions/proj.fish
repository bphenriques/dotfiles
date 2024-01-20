function proj -d 'Interactive look for projects under PROJ_ROOT'
  set -l target (projf $argv[1])
  and cd $target
end
