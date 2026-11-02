---
name: meal-planning
description: Plan the week's meals from the recipe library and Cookidoo, and change the plan as the week unfolds.
version: 5.0.0
metadata:
  hermes:
    category: productivity
---

# Meal planning

## When to Use

The user asks what to cook this week, for a meal plan over several days, about a change to the week (guests, a
dinner out, another dish), or about the "Cozinha" (kitchen).

## Procedure

Every `meals` tool returns text for the user. Reply with it exactly as returned, table and links included, then at
most one short question. Never write or change a plan yourself, nor suggest dishes the tools did not return.

Turn what the user says into wishes, in their own words: a food as they name it ("salmão", "salmon"), a day as
they say it ("quinta", "29/10"). One message may hold several; pass them all in one call.

| The user says | Call |
|---|---|
| plan the week, show the plan | `plan_week` |
| plan it with wishes, e.g. "esta semana sem tempo", "quero salmão na quinta", "algo com grão" | `plan_week` with `wishes`: a day for each that names one, none for the whole week |
| other options for the whole week | `plan_week` with `different: true` |
| a change to the week: "troca a quarta", "quarta peixe", "quinta com salmão", "sábado temos 6 à mesa", "sexta jantamos fora", "quarta há restos do almoço", "afinal jantamos em casa" | `adjust_week` with one change per day said |
| a dish not in the library for a day, e.g. "quarta faz X" | `adjust_week` with `recipe` if `find_recipes` finds X, else the `recipe-new` skill |
| change a soup | `swap_soup` with a day it covers |
| save the plan shown in this conversation: "guarda", "confirmo", "está bom", "podes adicionar", "sim" after "Guardo este plano?", "faz as compras" | `commit_plan`: it also fills My week and the shopping list |
| "gostámos do X", "não voltes a pôr X", forget that | `rate_dish` with `liked`, `never` or null |
| a Cookidoo recipe is missing, "sincroniza o Cookidoo" | `sync_cookidoo` |
| what is on the shopping list, "o que vou comprar" | `groceries_preview` |
