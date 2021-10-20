HOST_TARGET := $(shell cat $(CURDIR)/.host)

# Installs all the required dependencies.
.PHONY: bootstrap
bootstrap:
	@$(CURDIR)/scripts/bootstrap.sh

# Runs static analysis
.PHONY: lint
lint:
	@nix-shell --packages shellcheck --run "shellcheck -P $(CURDIR)/ **/*.sh"

# Checks for missing dependencies
.PHONY: doctor
doctor:
	@$(CURDIR)/scripts/doctor.sh

# Updates both flake.lock and Doom emacs
.PHONY: update
update: doctor
	@$(CURDIR)/scripts/update.sh

########################
#    Main Targets      #
########################

.PHONY: sync
sync: doctor
	# Building...
	@echo "Syncing $(HOST_TARGET)"
	@nix build .#$(HOST_TARGET)
	# Applying...
	@$(CURDIR)/result/sw/bin/darwin-rebuild switch --flake .#$(HOST_TARGET)
	# Syncing Doom Emacs...
	@$(CURDIR)/scripts/sync-doom-emacs.sh
