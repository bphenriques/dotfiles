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
- [drafts.py](./tools/drafts/drafts.py): drafts recipes from pages ([recipe-scrapers](https://github.com/hhursev/recipe-scrapers)), videos (yt-dlp, whisper on ai) or text, in Portuguese, and `aisle.conf` entries, into `drafts/`; `promote` moves them in once reviewed. The lifecycle is in the recipes `AGENTS.md`.
- [cook-mcp](https://github.com/cook-md/cook-mcp): searches and reads recipe files.

Tests run on build: a failing test blocks the deploy.

| to | do |
|---|---|
| change household or collection | edit dotfiles-private, push, `nix flake update dotfiles-private`, deploy |
| sign in to Cookidoo | `sudo -u hermes cookidoo login` on the VM |
| check planning | `sudo -u hermes meals propose` |
| draft an import by hand | `sudo -u hermes drafts new <url>` |
| replace a library recipe | copy it into `drafts/`, edit it, then `sudo -u hermes drafts promote "<new name>" --replace "<name>"` |
| complete aisle.conf | `sudo -u hermes drafts aisle`, then `drafts promote aisle.conf` |
| sync now | `sudo systemctl start cookidoo-sync` |
