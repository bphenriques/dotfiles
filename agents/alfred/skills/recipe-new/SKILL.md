---
name: recipe-new
description: Add a recipe the user describes to the recipe library, asking for what is missing.
version: 2.2.0
metadata:
  hermes:
    category: productivity
---

# New recipe

## When to Use

The user describes a recipe of their own, or wants a dish that is not in the library yet, e.g. "cria uma receita",
"quarta temos convidados, faz um folhado de pato". For a link or a video, use `recipe-import` instead.

## Procedure

Follow the lifecycle in the recipes `AGENTS.md`: code makes the draft from the user's words, and you never write or
edit it.

1. Ask in one message for whatever is missing: the name, the category (`mains`, `soups`, `desserts`, `breakfast` or
   `baby-food`), servings, total time in minutes, difficulty, each ingredient with its quantity, and the steps.
   Never guess a quantity.
2. `search_recipes` to check it is not in the library already.
3. Pass the recipe in the user's words, adding nothing:
   ```
   ${HERMES_SKILL_DIR}/scripts/drafts new --text --name "<name>" --category <category> <<'EOF'
   <servings> porções, <time> minutos, <difficulty>.
   Ingredientes: <each with its quantity>.
   Passos: <the steps>.
   EOF
   ```
4. Relay its message exactly as printed.
5. When the user approves, `${HERMES_SKILL_DIR}/scripts/drafts promote "<name>"` and relay what it prints. If it
   names ingredients missing from `config/aisle.conf`, offer the `recipe-aisle` skill.
6. If it is for a day of the meal plan, `swap_day` with that day and the recipe's name.
