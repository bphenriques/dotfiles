function ffd -d 'Interactive find folders using fd'
  set -l target (ffd-print $argv[1])
  and cd $target
end
