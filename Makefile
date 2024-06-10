ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY=s,sync
s sync:
	"$(ROOT_DIR)"/bin/sync.sh

.PHONY=u,update
u update:
	"$(ROOT_DIR)"/bin/update.sh

.PHONY=d,doctor
d doctor:
	"$(ROOT_DIR)"/bin/doctor.sh

.PHONY=r,repair
r repair:
  sudo nix-store --repair --verify --check-contents

# Improvements blocked by https://github.com/NixOS/nix/issues/6129
.PHONY=changelog
changelog:
  # Improvements are blocked by https://github.com/NixOS/nix/issues/6129
  nix profile diff-closures --profile /nix/var/nix/profiles/system
