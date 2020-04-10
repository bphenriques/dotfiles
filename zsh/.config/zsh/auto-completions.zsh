zstyle ':completion:*' menu select=2        # Makes sure that tab-completion is avaialble iff number_items > 2

autoload -Uz compinit && compinit           # Generate auto completion
