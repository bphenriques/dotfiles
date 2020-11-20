# Load ZSH Plugin Manager
test ! -d "$ZDOTDIR/.zinit" && sh -c "$(curl -fsSL https://raw.githubusercontent.com/zdharma/zinit/master/doc/install.sh)"
source "$ZDOTDIR/.zinit/bin/zinit.zsh"

# Load Plugins
zinit light zdharma/fast-syntax-highlighting
zinit ice depth=1; zinit light romkatv/powerlevel10k

# Load Theme - https://github.com/romkatv/powerlevel10k
source "$ZDOTDIR/powerlevel10k.theme.zsh"
