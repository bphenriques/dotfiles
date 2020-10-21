MAKEFILE_LOCATION = $(CURDIR)

# Ignore Array related checks and the source path.
lint:
	shellcheck -P SCRIPTDIR/ **/*.sh

# Manually stow files: make module=<module> stow
stow:
	stow --dir $(MAKEFILE_LOCATION) --target $(HOME) $(module)

# Check for bogus links
doctor:
	chkstow --target $(HOME) --badlinks 2>/dev/null | grep -v ".*/Library/.*"

install-macos:
	sh installer.sh git emacs scala terminal utils zsh tmux docker macos

install-macos-personal:
	sh installer.sh git emacs scala terminal utils zsh tmux docker macos-personal
