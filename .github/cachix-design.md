# Targeted binary cache design

## Status

Design only. The workflow and cache have not been created yet.

## Goal

Move known expensive, public Nix builds from the laptop to GitHub Actions. A
normal flake update should become:

1. Update and push `flake.lock`.
2. Wait for the `cache-warm` check.
3. Update the laptop and download the cached outputs.

The initial package list is deliberately static:

| Input | Output | Reason |
| --- | --- | --- |
| `omp` | `packages.x86_64-linux.default` | Upstream does not publish this output to a maintainer cache. |
| `hermes-agent` | `packages.x86_64-linux.default` | The upstream Cachix cache does not contain the pinned output. |

Add another package only after it repeatedly causes a meaningful local build.
Do not try to discover expensive packages by evaluating a complete host in CI.

## Why targeted builds

Most `nixos-unstable` packages are already built by Hydra and served by
`cache.nixos.org`. Local builds mainly come from independent flakes, overrides,
unsupported packages, and generated configuration.

A complete laptop closure is about 27 GiB, larger than the usable disk on a
standard GitHub-hosted runner. It also includes private host data. Building only
the known expensive packages keeps each job small and preserves the privacy
boundary.

The laptop NVMe is healthy: 1% wear, 100% available spare, about 15 TB written,
and no reported media errors. Caching is therefore an improvement to update
latency, CPU use, and future writes, not a response to imminent drive failure.

## Cache ownership and trust

Use a public Cachix cache owned by the dotfiles owner. Public access is safe for
these outputs because both package sources and their pinned revisions are
already public.

Nix store paths are normally input-addressed. Their names do not independently
prove the downloaded contents were produced by the expected derivation. Adding
a cache signing key means trusting that key to supply store contents. A cache
controlled by this repository and its protected GitHub Actions workflow keeps
that trust within the same administrative boundary.

Existing upstream caches do not remove the need for this cache:

- OMP declares `nix-community.cachix.org`, but its pinned OMP output is absent.
  This is a broad nix-community cache, not an OMP maintainer cache.
- Hermes has `hermes-agent.cachix.org`, created by a NousResearch team member,
  but its pinned output is absent and current upstream CI does not appear to
  populate it.
- Do not add an arbitrary third-party cache solely because it contains a wanted
  path.

## Privacy boundary

The workflow must never fetch, evaluate, build, or upload:

- `dotfiles-private`
- `selfhost-nix`
- any `nixosConfigurations` or Home Manager activation package
- any package or closure derived from private host configuration

The workflow builds the public input flakes directly from revisions recorded in
the public `flake.lock`. It must not run `nix build .#...`, `nix flake check`, or
another command that evaluates the root fleet flake.

Only the explicit OMP or Hermes result closure is pushed. Do not use
`cachix watch-store`, a Cachix daemon, or an unrestricted store upload. A public
cache will reveal package names, versions, and public dependencies, but no host
configuration or secrets.

## Workflow

Create `.github/workflows/cache-warm.yml` with these properties:

- Trigger on pushes to the default branch that change `flake.lock` or the
  workflow, plus `workflow_dispatch` for retries.
- Do not run on `pull_request_target`.
- Set `permissions: contents: read`.
- Use one matrix job per package so OMP and Hermes get separate runner disks.
- Pin every third-party action to a full commit SHA.
- Read the input owner, repository, and revision from `flake.lock`. Never build a
  moving branch or tag.
- Install Cachix without enabling automatic store watching.
- Build with `--print-out-paths --no-link`, then push only those output paths and
  their closures.
- Make the Cachix authentication token available only to trusted default-branch
  and manually dispatched jobs. GitHub must not expose it to forked pull
  requests.

OMP intentionally retains its upstream nixpkgs because the root flake does not
make that input follow the fleet's nixpkgs. Build the exact locked OMP revision
using its own lock file.

Hermes follows the fleet's root `nixpkgs`. Build the exact locked Hermes revision
with `--override-input nixpkgs` set to the exact root nixpkgs revision from
`flake.lock`. This makes the cached output match the one used by the fleet
without evaluating the private root flake.

The conceptual commands are:

```sh
# Values are extracted from flake.lock, not hard-coded.
nix build "github:can1357/oh-my-pi/$OMP_REV#default" \
  --print-out-paths --no-link

nix build "github:NousResearch/hermes-agent/$HERMES_REV#default" \
  --override-input nixpkgs "github:NixOS/nixpkgs/$NIXPKGS_REV" \
  --print-out-paths --no-link

# Run for only the output paths printed by the selected build.
cachix push <cache-name> <output-path>
```

The implementation should derive repository names as well as revisions from the
lock file so input changes do not silently keep building the old repository.

## Update behavior

Every lock update creates new store paths. An older cache entry cannot satisfy a
new revision, so GitHub Actions must finish before the first frictionless local
update. If the cache job has not run or failed, Nix remains correct and falls
back to a local build.

No periodic schedule is needed. A locked revision is immutable, and rebuilding
it periodically creates no new value. Use `workflow_dispatch` to retry a failed
or evicted entry. Cachix storage is independent of GitHub Actions cache
retention.

The `cache-warm` result should be a required or visibly checked status for lock
updates. The local update command does not need additional cache orchestration.

## Failure handling

- If one package exceeds runner disk capacity, first reduce that package to the
  variant actually used by the host. Otherwise use a larger hosted runner for
  that matrix entry only.
- Do not fall back to building on a laptop or home VM automatically.
- If a public Cachix account becomes unsuitable, the fallback is a private
  Attic instance on a non-laptop machine reachable through Tailscale. That adds
  storage, backup, authentication, and availability maintenance, so it is not
  the initial design.
- A compromised Cachix token or trusted workflow can poison the cache. Rotate
  the token and cache signing key after compromise, then rebuild the entries
  from reviewed revisions.
