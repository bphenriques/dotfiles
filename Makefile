# Installs all the required dependencies.
.PHONY: bootstrap
bootstrap:
	@$(CURDIR)/scripts/bootstrap.sh

.PHONY: sync
sync: doctor
	@$(CURDIR)/scripts/sync.sh

# Updates both flake.lock and Doom emacs
.PHONY: update
update: doctor
	@$(CURDIR)/scripts/update.sh

# Checks for missing dependencies
.PHONY: doctor
doctor:
	@$(CURDIR)/scripts/doctor.sh

.PHONY: test
test:
	@nix flake check

# Runs static analysis
.PHONY: lint
lint:
	@nix-shell --packages shellcheck --run "shellcheck -P $(CURDIR)/ **/*.sh"

# Util for convenience
.PHONE: changelog
changelog:
	nix profile diff-closures --profile /nix/var/nix/profiles/system
