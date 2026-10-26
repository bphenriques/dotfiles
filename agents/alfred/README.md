# Alfred

Deployed by [agent-alfred-vm](../../hosts/guests/agent-alfred-vm). `SOUL.md` gets the private `household.md`
appended; `environment.md` becomes the workspace `AGENTS.md`.

- The tool policy in [default.nix](./default.nix) steers the model but is not a boundary (`terminal` runs anything
  the `hermes` user can). The VM is.
- Skills are an allowlist (`skills` in [default.nix](./default.nix)).

## Meals ("Cozinha")

- [meals.py](./tools/meals/meals.py): MCP server that plans the week from the recipe library, saves plans to
  `plans/<date>.menu`; saving also fills My week and the shopping list. Rules live in the code, so the model only relays its replies.
- [cookidoo.py](./tools/cookidoo/cookidoo.py): the only Cookidoo client. Syncs the collection into the library daily, and
  writes My week and the shopping list, never touching your own entries.
- [cook-mcp](https://github.com/cook-md/cook-mcp): reads, imports and writes recipe files.

Tests run on build: a failing test blocks the deploy.

| to | do |
|---|---|
| change household or collection | edit dotfiles-private, push, `nix flake update dotfiles-private`, deploy |
| sign in to Cookidoo | `sudo -u hermes cookidoo login` on the VM |
| check planning | `sudo -u hermes meals propose` |
| sync now | `sudo systemctl start cookidoo-sync` |
