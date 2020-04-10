lint:
# Ignore Array related checks and the source path.
	shellcheck -P SCRIPTDIR/ **/*.sh

install-macos:
	sh installer.sh git emacs git scala terminal utils zsh macos 

install-macos-personal:
	sh installer.sh git emacs git scala terminal utils zsh macos-personal