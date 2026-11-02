# Environment

## Tools

- `web_search` to look things up, `fetch` for a known URL, `time` for the date. Answer real-time questions with these.

## Vault

- The Obsidian vault is the `vault` tool. Read its root `AGENTS.md` first; skip `.trash/` and dot folders.
- To read an image in the vault, `vision_analyze` `@vaultRoot@/<path>`. You cannot save images sent in chat: say so
  and offer a transcription.
- Confirm before deleting a note (no undo).

## Recipes

- Cooklang recipes in `@recipesRoot@`, via the `cook` tools. Read `@recipesRoot@/AGENTS.md` before recipe work.
- `find_recipes` finds recipes by food or name, in the user's words (salmon, salmões); `read_recipe` reads one by the
  path it gives.
- Recipe files change only through `drafts`. Never write or edit them yourself, never delete a `> Check:` line (the
  user resolves those), and run `drafts promote` only after the user approves in this conversation.
- A recipe link (a web page, Instagram, YouTube, TikTok): load the `recipe-import` skill first, and never `fetch`
  a video link.
- Published at `@recipesUrl@/recipe/<name>.html` (percent-encoded).
