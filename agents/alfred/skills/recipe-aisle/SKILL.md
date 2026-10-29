---
name: recipe-aisle
description: Complete the recipes' config/aisle.conf, the shopping list's sections and ingredient names, with the ingredients it lacks.
version: 1.1.0
metadata:
  hermes:
    category: productivity
---

# Complete aisle.conf

## When to Use

The user asks to complete or tidy `config/aisle.conf`, or a promoted recipe has ingredients it lacks.

## Procedure

1. Run `${HERMES_SKILL_DIR}/scripts/drafts aisle` in the foreground with the terminal's `timeout` set to exactly 600.
2. Relay its message exactly as printed. The user reviews `drafts/aisle.conf`; never edit it yourself.
3. When the user approves, `${HERMES_SKILL_DIR}/scripts/drafts promote aisle.conf` and relay what it prints.
