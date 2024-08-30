function _frg_widget -d 'frg interactive widget'
  set target (frg)
  and commandline --current-token --replace -- $target
  and commandline --function repaint
end
