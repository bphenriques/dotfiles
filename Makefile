ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))


.PHONY=s,sync
s sync:
	"$(ROOT_DIR)"/bin/sync.sh

.PHONY=u,update
u update:
	"$(ROOT_DIR)"/bin/update.sh

.PHONY=r,repair
r repair:
	sudo nix-store --repair --verify --check-contents

fmt format:
	nix-shell -p fd nixpkgs-fmt --command "fd -e nix -E '/nix/sources.nix' -E 'hardware-configuration*' -x nixpkgs-fmt \"{}\" \;"

# Improvements blocked by https://github.com/NixOS/nix/issues/6129
.PHONY=changelog
changelog:
	# Improvements are blocked by https://github.com/NixOS/nix/issues/6129
	# nix profile diff-closures --profile /nix/var/nix/profiles/system
