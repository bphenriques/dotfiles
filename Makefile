# Installs all the required dependencies.
.PHONY: bootstrap
bootstrap:
	@$(CURDIR)/bin/bootstrap.sh

.PHONY: sync
sync: doctor
	@$(CURDIR)/bin/sync.sh

# Updates both flake.lock and Doom emacs
.PHONY: update
update: doctor
	@$(CURDIR)/bin/update.sh

# Checks for missing dependencies
.PHONY: doctor
doctor:
	@$(CURDIR)/bin/doctor.sh

.PHONY: test
test:
	@nix flake check

# Runs static analysis
.PHONY: lint
lint:
	@nix-shell --packages shellcheck --run "shellcheck -P $(CURDIR)/ **/*.sh"

# Util for convenience
.PHONY: changelog
changelog:
	nix profile diff-closures --profile /nix/var/nix/profiles/system
