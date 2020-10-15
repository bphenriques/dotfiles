target="$(proj $(tmux display-message -p '#{window_name}'))"

if [ ! -z "$target" ]; then 
   tmux rename-window -t $(tmux display-message -p '#I') "$target$(basename $PWD)"
fi
