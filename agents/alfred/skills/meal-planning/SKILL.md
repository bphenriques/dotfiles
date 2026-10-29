---
name: meal-planning
description: Plan the week's meals from the recipe library and Cookidoo, before the groceries.
version: 4.0.0
metadata:
  hermes:
    category: productivity
---

# Meal planning

## When to Use

The user asks what to cook this week, for a meal plan over several days, or about the "Cozinha" (kitchen).

## Procedure

Every `meals` tool returns text for the user. Reply with it exactly as returned, links included, then at
most one short question. Never write or change a plan yourself.

| The user says | Call |
|---|---|
| plan the week, show the plan | `propose_week` |
| other options for the whole week | `propose_week` with `different: true` |
| short of time, e.g. "esta semana estou sem tempo", "só pratos fáceis" | `propose_week` with `max_minutes` (30 when unsaid) and/or `difficulty: "easy"` |
| change a day or other options for it, e.g. "troca a quarta", "quarta peixe", "quarta o Arroz de Pato", "quarta algo rápido" | `swap_day` with that day, and `protein`, `recipe`, `max_minutes` or `difficulty` if said |
| a dish not in the library for a day, e.g. "quarta temos convidados, faz X" | the `recipe-new` skill |
| change a soup | `swap_soup` with a day it covers |
| save the plan shown in this conversation: "guarda", "confirmo", "está bom", "podes adicionar", "sim" after "Guardo este plano?", "faz as compras" | `commit_plan`: it also fills My week and the shopping list |
| "gostámos do X", "não voltes a pôr X", forget that | `rate_dish` with `liked`, `never` or null |
| a Cookidoo recipe is missing, "sincroniza o Cookidoo" | `sync_cookidoo` |
| what is on the shopping list, "o que vou comprar" | `groceries_preview` |
