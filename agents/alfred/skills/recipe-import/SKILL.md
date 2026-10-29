---
name: recipe-import
description: Import a recipe from a web page, YouTube, Instagram or TikTok link into the recipe library.
version: 3.3.0
metadata:
  hermes:
    category: productivity
---

# Import a recipe

## When to Use

The user sends a link to a recipe (a web page, or a YouTube, Instagram or TikTok video) to keep it.

## Procedure

Follow the lifecycle in the recipes `AGENTS.md`: code makes the draft, the user resolves its checks, and you never
write or edit it.

1. Run `${HERMES_SKILL_DIR}/scripts/drafts new <url>` in the foreground with the terminal's `timeout` set to exactly
   600 (its maximum: more sends it to the background).
   - "No recipe here": relay it, and offer the `recipe-new` skill.
   - "A draft … exists already": ask whether to review it or delete it to start over.
2. Relay its message exactly as printed; you may show the photo. If a check says the caption links to a recipe page,
   offer to draft from that page instead.
3. When the user approves, `${HERMES_SKILL_DIR}/scripts/drafts promote "<name>"` and relay what it prints. If it names
   ingredients missing from `config/aisle.conf`, offer the `recipe-aisle` skill.
