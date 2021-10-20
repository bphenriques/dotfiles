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

.PHONY: sync-personal-macos
sync-personal-macos: doctor
	# Building...
	@nix build .#personal-macos
	# Applying...
	@$(CURDIR)/result/sw/bin/darwin-rebuild switch --flake .#personal-macos
	# Syncing Doom Emacs...
	@$(CURDIR)/scripts/sync-doom-emacs.sh

.PHONY: sync-work-macos
sync-work-macos: doctor
	# Building...
	@nix build .#work-macos
	# Applying...
	@$(CURDIR)/result/sw/bin/darwin-rebuild switch --flake .#work-macos
	# Syncing Doom Emacs...
	@$(CURDIR)/scripts/sync-doom-emacs.sh
