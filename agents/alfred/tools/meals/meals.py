"""The household meal planner: an MCP server over the Cooklang recipe library that applies the household rules,
keeps the current plan, and hands groceries to the `cookidoo` program."""
import argparse
import asyncio
import json
import random
import re
import subprocess
import unicodedata
from datetime import date, timedelta
from pathlib import Path
from urllib.parse import quote

from mcp.server.fastmcp import FastMCP

PLAN = Path("@planFile@")
LIBRARY = Path("@library@")
SITE = "@site@"
COOK = "@cook@"
COOKIDOO = "@cookidoo@"
COOKIDOO_TIMEOUT = 120   # seconds; a hung network must not hang the chat

# Whole words (plurals too) in ingredient names, Portuguese and English. Animals and processed meats only: cuts such
# as "lombo" name fish as often as meat. A recipe's `protein:` metadata wins over these.
MEAT = ("frango", "peru", "pato", "porco", "vaca", "novilho", "vitela", "borrego", "cabrito", "coelho", "entrecosto",
        "carne", "febra", "chouriço", "bacon", "presunto", "fiambre", "salsicha", "alheira",
        "chicken", "turkey", "duck", "pork", "beef", "veal", "lamb", "ham", "sausage", "chorizo", "mince")
FISH = ("bacalhau", "salmão", "atum", "peixe", "pescada", "dourada", "robalo", "sardinha", "cavala", "carapau", "truta",
        "camarão", "camarões", "gamba", "polvo", "lula", "marisco", "amêijoa", "mexilhão", "mexilhões",
        "fish", "salmon", "tuna", "cod", "hake", "prawn", "shrimp", "squid", "octopus", "sardine", "mackerel", "trout")

# What the tools return is shown to the user as it is, so its labels are the household's language.
WEEKDAYS = ("seg", "ter", "qua", "qui", "sex", "sáb", "dom")
PROTEINS = {"meat": "carne", "fish": "peixe", "both": "carne e peixe", "vegetarian": "vegetariano", "unknown": "?"}
SOUP_DAYS = 3
RATINGS = ("liked", "never")   # tags, which the Cookidoo sync keeps on its copies
MEAL_SERVINGS = 2   # one per adult; a main lasts as many days as its servings cover


def words(*terms):
    return re.compile(r"\b(?:" + "|".join(map(re.escape, terms)) + r")(?:s|es)?\b", re.IGNORECASE)


MEAT_RE, FISH_RE = words(*MEAT), words(*FISH)

mcp = FastMCP("meals")
plan_lock = asyncio.Lock()   # one change to the current plan at a time


def protein(ingredients, declared=None):
    if declared:
        return declared
    names = " | ".join(ingredients)
    meat, fish = bool(MEAT_RE.search(names)), bool(FISH_RE.search(names))
    return "both" if meat and fish else "meat" if meat else "fish" if fish else "vegetarian"


def read(path):
    """A recipe through cook's own parser."""
    return json.loads(subprocess.run([COOK, "recipe", "read", "--format", "json", str(path)],
                                     capture_output=True, text=True, check=True).stdout)


def library():
    """Soups and mains from the Cooklang library, Cookidoo copies included, and the files cook could not read
    (one bad file never stops planning)."""
    found = {"soup": [], "main": [], "broken": []}
    for f in sorted(LIBRARY.glob("*.cook")):
        try:
            recipe = read(f)
        except subprocess.CalledProcessError:
            found["broken"].append(f.stem)
            continue
        meta = recipe["metadata"]["map"]
        tags = meta.get("tags") or []
        course = "soup" if "soups" in tags else "main" if "mains" in tags else None
        if course is None:
            continue
        found[course].append({
            "source": "cookidoo" if meta.get("cookidoo") else "library", "id": f.stem, "cookidoo_id": meta.get("cookidoo"), "name": f.stem,
            "url": f"{SITE}/recipe/{quote(f.stem)}.html",
            "protein": protein([i["name"] for i in recipe["ingredients"]], meta.get("protein")),
            "minutes": meta.get("time"), "difficulty": meta.get("difficulty"), "servings": meta.get("servings"),
            "rating": next((t for t in tags if t in RATINGS), None),
        })
    return found


async def cookidoo(*args, order=None):
    """Run the cookidoo program and return its JSON; its one-line error becomes the exception."""
    try:
        done = await asyncio.to_thread(subprocess.run, [COOKIDOO, *args], input=order and json.dumps(order),
                                       capture_output=True, text=True, timeout=COOKIDOO_TIMEOUT)
    except subprocess.TimeoutExpired:
        raise RuntimeError(f"cookidoo {args[0]}: sem resposta do Cookidoo em {COOKIDOO_TIMEOUT} s") from None
    if done.returncode:
        raise RuntimeError(done.stderr.strip().splitlines()[-1] if done.stderr.strip() else f"cookidoo {args[0]} failed")
    return json.loads(done.stdout)


async def gather(span):
    """The soups and mains to choose from (the library alone), what the user's My week already holds for each day of
    the span, and notes: a file left out, or Cookidoo unreachable (the plan is still made, without My week)."""
    found = await asyncio.to_thread(library)
    known = {r["cookidoo_id"]: r for r in found["soup"] + found["main"] if r["cookidoo_id"]}
    notes = [f"A receita {name} não pôde ser lida e ficou de fora." for name in found["broken"]]
    planned, ours = {}, placed()
    try:
        for day, recipes in (await cookidoo("week", span[0].isoformat(), str(len(span)))).items():
            theirs = [r for r in recipes if [day, r["id"]] not in ours]   # what a commit put there is the plan, not a fixed day
            if not theirs:
                continue
            r = theirs[0]
            planned[day] = known.get(r["id"]) or {
                "source": "cookidoo", "id": r["id"], "cookidoo_id": r["id"], "name": r["name"], "url": r["url"],
                "protein": "unknown", "minutes": None, "difficulty": None, "servings": None}
    except RuntimeError as e:
        notes.append(f"Sem acesso ao Cookidoo ({e}); o plano não tem em conta o My week.")
    return found["soup"], found["main"], planned, notes, recent(span[0])


def recent(start, days=14):
    """The dishes of the plans committed in the two weeks before `start`, which a new plan avoids when it can."""
    found = set()
    for f in (LIBRARY / "plans").glob("*.menu"):
        try:
            begun = date.fromisoformat(f.stem)
        except ValueError:
            continue
        if start - timedelta(days) <= begun < start:
            found |= set(re.findall(r"@\./(.+?)\{", f.read_text()))
    return found


def clash(a, b):
    """Two neighbouring mains that break a rule: the same protein, or both hard."""
    if a is None or b is None:
        return False
    meat = a["protein"] in ("meat", "both") and b["protein"] in ("meat", "both")
    fish = a["protein"] in ("fish", "both") and b["protein"] in ("fish", "both")
    hard = a["difficulty"] == "hard" and b["difficulty"] == "hard"
    return meat or fish or hard


def lasts(r):
    """How many days a main dish feeds the household."""
    return max(1, (r["servings"] or MEAL_SERVINGS) // MEAL_SERVINGS)


def compose(span, soups, mains, planned, keep_mains, keep_soups, exclude, force=None, recent=frozenset()):
    """A plan that keeps the fixed days and fills the rest by the household rules, each main followed by its leftovers
    for as long as it lasts. Seeded by the start day, so the same inputs give the same plan and a change touches only
    what it names. `force` is {date: protein} the user asked for, which wins over the rules. Dishes in `recent` (the
    last two weeks) come last among those that fit."""
    force = force or {}
    rng = random.Random(span[0].isoformat())
    by_id = {r["id"]: r for r in soups + mains}
    pool = [r for r in mains if r["id"] not in exclude and r.get("rating") != "never"]
    soup_pool = [r for r in soups if r["id"] not in exclude and r.get("rating") != "never"]

    def rank(r):
        return r["id"] in recent, r.get("rating") != "liked"
    rng.shuffle(pool)
    rng.shuffle(soup_pool)
    notes = []

    chosen, fixed, leftover = [None] * len(span), [None] * len(span), [False] * len(span)
    for i, d in enumerate(span):
        if d.isoformat() in planned:
            chosen[i], fixed[i] = planned[d.isoformat()], "cookidoo"
        elif keep_mains.get(d.isoformat()) in by_id:
            chosen[i], fixed[i] = by_id[keep_mains[d.isoformat()]], "kept"
    used = {r["id"] for r in chosen if r} | {s for s in keep_soups.values() if s in by_id}

    def cover(i, r):
        """The days from day i that dish r covers: as many as it lasts, up to the next day already taken."""
        n = 1
        while n < lasts(r) and i + n < len(span) and chosen[i + n] is None:
            n += 1
        return n

    def place(i, r):
        n = cover(i, r)
        for j in range(i, i + n):
            chosen[j], leftover[j] = r, j > i
        used.add(r["id"])
        return n

    def unplace(i, n):
        used.discard(chosen[i]["id"])
        for j in range(i, i + n):
            chosen[j], leftover[j] = None, False

    def after(i, r):
        """The dish eaten right after r's days, were r cooked on day i."""
        j = i + cover(i, r)
        return chosen[j] if j < len(span) else None

    def options(i):
        """Unused dishes for day i (the forced protein only, if any), and those of them that follow the rules,
        the wanted protein first."""
        d = span[i].isoformat()
        prev = chosen[i - 1] if i else None
        free = [r for r in pool if r["id"] not in used and r["protein"] == force.get(d, r["protein"])]
        last = next((c["protein"] for c in reversed(chosen[:i]) if c and c["protein"] in ("meat", "fish")), "fish")
        want = force.get(d) or ("fish" if last == "meat" else "meat")
        fits = sorted((r for r in free if not clash(r, prev) and not clash(r, after(i, r))), key=lambda r: (r["protein"] != want, *rank(r)))
        return fits, free, want

    def fill(i):
        """Backtracking: a greedy pass can corner itself (hard, then hard or same protein) when a valid plan exists."""
        if i == len(span):
            return True
        if chosen[i]:
            return fill(i + 1)
        for r in options(i)[0][:6]:
            n = place(i, r)
            if fill(i + n):
                return True
            unplace(i, n)
        return False

    for i in range(len(span)):   # a fixed day's leftovers come first, so the dishes around them are chosen to fit
        if fixed[i]:
            place(i, chosen[i])

    if not fill(0):   # no plan follows every rule: fill in order and say where it breaks
        for i, d in enumerate(span):
            if chosen[i]:
                continue
            fits, free, want = options(i)
            pick = next(iter(fits), None) or next(iter(free), None)
            if pick is None:
                notes.append(f"{label(d)}: não há mais pratos de {PROTEINS.get(want, want)} por usar.")
                continue
            if not fits:
                notes.append(f"{label(d)}: {pick['name']} fica junto de outro prato de {PROTEINS[pick['protein']]} ou difícil.")
            place(i, pick)

    blocks = []
    for b in range(0, len(span), SOUP_DAYS):
        start, end = span[b], span[min(b + SOUP_DAYS, len(span)) - 1]
        fresh = sorted((r for r in soup_pool if r["id"] not in used), key=rank)
        soup = by_id.get(keep_soups.get(start.isoformat())) or next(iter(fresh), None)
        if soup:
            used.add(soup["id"])
        blocks.append({"start": start.isoformat(), "end": end.isoformat(), "soup": soup})

    alternatives = {p: sorted((r for r in pool if r["id"] not in used and r["protein"] == p), key=rank)[:3]
                    for p in ("meat", "fish", "vegetarian")}
    days = [{"date": d.isoformat(), "main": chosen[i], "fixed": fixed[i], "leftover": leftover[i]} for i, d in enumerate(span)]
    return {"days": days, "soups": blocks, "alternatives": alternatives, "notes": notes}


def label(d):
    d = date.fromisoformat(d) if isinstance(d, str) else d
    return f"{WEEKDAYS[d.weekday()]} {d:%d/%m}"


def link(r):
    return f"[{r['name']}]({r['url']})" + (" (Cookidoo)" if r["source"] == "cookidoo" else "")


def soup_starts(plan):
    """{date: soup} for the day each soup batch is made."""
    return {b["start"]: b["soup"] for b in plan["soups"] if b["soup"]}


def menu(plan):
    """The plan as a Cooklang .menu file: library dishes as references the site links and cook can shop from,
    anything else (a My week dish not in the library) as plain text."""
    days, soups = plan["days"], soup_starts(plan)
    lines = ["---", f'description: "Plano de refeições {label(days[0]["date"])} a {label(days[-1]["date"])}"', "---"]
    for d in days:
        dishes = [r for r in (d["main"], soups.get(d["date"])) if r]
        items = [f"- sobras: {r['name']}" if r is d["main"] and d.get("leftover")   # text, so the dish is shopped for once
                 else f"- @./{r['id']}{{}}" if r["id"] != r["cookidoo_id"] else f"- {r['name']} (Cookidoo)"   # a My week dish not in the library
                 for r in dishes] or ["- por decidir"]
        lines += ["", f"== {label(d['date'])} ({d['date']}) ==", "", "Jantar: \\", *[f"{i} \\" for i in items[:-1]], items[-1]]
    return "\n".join(lines) + "\n"


def menu_path(state):
    return LIBRARY / "plans" / f"{state['start']}.menu"


def write_menu(state):
    """Write the plan's .menu, only when it changed, so the site rebuilds only on a real change."""
    path, text = menu_path(state), menu(state["plan"])
    if not path.exists() or path.read_text() != text:
        path.parent.mkdir(exist_ok=True)
        tmp = path.with_name(f".{path.name}.tmp")
        tmp.write_text(text)
        tmp.replace(path)


def summary(state):
    """What Alfred says: one line per day with the dish names, then the plan's page once it is committed (the links
    live there), or the question whether to commit it."""
    plan, soups, lines = state["plan"], soup_starts(state["plan"]), []
    for d in plan["days"]:
        main, soup = d["main"], soups.get(d["date"])
        dish = "por decidir" if not main else f"sobras de {main['name']}" if d.get("leftover") else main["name"]
        lines.append(f"{label(d['date'])}: {dish}" + (f" · sopa: {soup['name']}" if soup else ""))
    lines += [f"Nota: {n}" for n in plan["notes"]]
    if state.get("committed"):
        lines += ["", f"Plano guardado: {SITE}/menu/plans/{quote(state['start'])}.html"]
    else:
        lines += ["", "Rascunho, ainda não guardado. Guardo este plano?"]
    return "\n".join(lines)


def my_week_dishes(state):
    """[date, Cookidoo id] for each Cookidoo dish the plan cooks on a day the user's My week left free."""
    return [[d["date"], d["main"]["cookidoo_id"]] for d in state["plan"]["days"]
            if d["main"] and d["main"]["cookidoo_id"] and d["fixed"] != "cookidoo" and not d.get("leftover")]


async def commit(state):
    """Save the plan to the library, put its Cookidoo dishes in My week and its groceries on the shopping list,
    replacing what an earlier commit put in either. `in_week` and `shopping` hold what commits put there, so the
    user's own entries are never touched. Returns the saved state and the lines for the user."""
    span = {d.isoformat() for d in plan_span(state)}
    ours, want = state.get("in_week", []), my_week_dishes(state)
    stale = [p for p in ours if p[0] in span and p not in want]
    state, notes = {**state, "committed": True}, []
    write_menu(state)
    if want or stale:
        try:
            await cookidoo("plan", order={"add": want, "remove": stale})
            today = date.today().isoformat()
            state["in_week"] = [p for p in ours if p[0] not in span and p[0] >= today] + want
            notes += [f"No My week: {len(want)} prato(s) do Cookidoo."] if want else []
        except RuntimeError as e:
            notes.append(f"O My week não foi atualizado ({e}); guarda de novo para tentar outra vez.")
    cookidoo_recipes, items = await asyncio.to_thread(groceries, state)
    order = {"recipes": [r["cookidoo_id"] for r in cookidoo_recipes], "items": [f"{name} {qty}".strip() for name, qty in items]}
    sent = state.get("shopping")
    try:
        if not sent or (sent["recipes"], sent["texts"]) != (order["recipes"], order["items"]):
            done = await cookidoo("send", order={**order, "sent": sent})
            state["shopping"] = {**done, "texts": order["items"]}
        notes.append(f"Na lista de compras do Cookidoo: ingredientes de {len(cookidoo_recipes)} receita(s) do Cookidoo "
                     f"e {len(items)} artigo(s).")
    except RuntimeError as e:
        notes.append(f"A lista de compras não foi atualizada ({e}); guarda de novo para tentar outra vez.")
    save(state)
    return state, notes


@mcp.tool(structured_output=False)
async def commit_plan() -> str:
    """Save the current plan to the recipe library, where the recipes site shows it with links, put its Cookidoo
    dishes in Cookidoo's My week and its groceries on Cookidoo's shopping list, and return the summary to relay as it
    is. Only when the user says to save or confirm it; a later change to the plan or its recipes needs saving again."""
    async with plan_lock:
        state, notes = await commit(load())
    return "\n".join([summary(state), *notes])


def load():
    if not PLAN.exists():
        raise RuntimeError("There is no plan yet: call propose_week first.")
    return json.loads(PLAN.read_text())


def save(state):
    tmp = PLAN.with_suffix(".tmp")
    tmp.write_text(json.dumps(state, ensure_ascii=False))
    tmp.replace(PLAN)


def placed():
    """The [date, Cookidoo id] pairs commits put in My week."""
    return load().get("in_week", []) if PLAN.exists() else []


def plan_span(state):
    return [date.fromisoformat(state["start"]) + timedelta(n) for n in range(state["days"])]


def plain(text):
    return unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode().casefold().strip()


def resolve(state, day):
    """A day of the current plan from what the user or the table says: 2026-10-28, 28/10, qua, quarta or 4f (4ª feira)."""
    dates = [d["date"] for d in state["plan"]["days"]]
    text = plain(day)
    nth = re.match(r"([2-6])\s*[af]", text)   # segunda-feira is the 2nd day, so 2f is Monday
    for iso in dates:
        d = date.fromisoformat(iso)
        named = text[:3] == plain(WEEKDAYS[d.weekday()]) or nth and int(nth[1]) - 2 == d.weekday()
        if text in (iso, f"{d:%d/%m}", f"{d.day}/{d.month}") or named:
            return iso
    raise ValueError(f"{day!r} is not a day of the current plan ({', '.join(map(label, dates))}).")


def cooked_on(state, iso):
    """The day the dish eaten on `iso` is cooked: that day, or the one whose leftovers it has."""
    days = state["plan"]["days"]
    i = next(n for n, d in enumerate(days) if d["date"] == iso)
    while i and days[i].get("leftover"):
        i -= 1
    return days[i]["date"]


async def replan(state, keep_mains, keep_soups, exclude, force=None, catalog=None):
    span = plan_span(state)
    soups, mains, planned, notes, recently = catalog or await gather(span)
    plan = compose(span, soups, mains, planned, keep_mains, keep_soups, set(exclude), force, recently)
    plan["notes"][:0] = notes
    state = {**state, "exclude": sorted(exclude), "plan": plan, "committed": False}
    save(state)
    return state


def kept(state, skip_day=None, skip_soup=None):
    plan = state["plan"]
    mains = {d["date"]: d["main"]["id"] for d in plan["days"] if d["main"] and not d.get("leftover") and d["date"] != skip_day}
    soups = {b["start"]: b["soup"]["id"] for b in plan["soups"] if b["soup"] and b["start"] != skip_soup}
    return mains, soups


@mcp.tool(structured_output=False)   # plain text for the user; a structured copy would only double it
async def propose_week(start: str | None = None, days: int = 7, different: bool = False) -> str:
    """The meal plan for the week: a short summary to relay as it is, with a link to the full plan. Shows the current plan if there
    is one for these days, and makes one otherwise.

    start: first day, YYYY-MM-DD (default today). days: how many days (default 7).
    different: true when the user wants other options: a new plan that avoids every dish of the current one.
    """
    async with plan_lock:
        first = date.fromisoformat(start) if start else date.today()
        if not different and PLAN.exists():
            current = load()
            if (current["start"], current["days"]) == (first.isoformat(), days):
                return summary(current)
        exclude = set()
        if different and PLAN.exists():
            old = load()
            exclude = set(old["exclude"]) | {d["main"]["id"] for d in old["plan"]["days"] if d["main"] and d["fixed"] != "cookidoo"}
            exclude |= {b["soup"]["id"] for b in old["plan"]["soups"] if b["soup"]}
        shopping = load().get("shopping") if PLAN.exists() and load()["start"] == first.isoformat() else None   # replaced on the next send
        state = await replan({"start": first.isoformat(), "days": days, "in_week": placed(), "shopping": shopping}, {}, {}, exclude)
    return summary(state)


def find(dishes, name):
    """The one dish whose name contains `name`."""
    matches = [r for r in dishes if plain(name) in plain(r["name"])]
    if len(matches) != 1:
        raise ValueError(f"{len(matches)} dishes match {name!r}: {[r['name'] for r in matches[:6]]}")
    return matches[0]


def retag(path, rating):
    """Set a recipe's rating tag (None clears it), checked through cook so a frontmatter it cannot rewrite is left as is."""
    tags = read(path)["metadata"]["map"].get("tags") or []
    want = [t for t in tags if t not in RATINGS] + ([rating] if rating else [])
    text = path.read_text()
    new = re.sub(r"(?m)^tags:.*$", lambda _: f"tags: {json.dumps(want, ensure_ascii=False)}", text, count=1)
    tmp = path.with_name(f".{path.name}.tmp")
    tmp.write_text(new)
    if read(tmp)["metadata"]["map"].get("tags") != want:
        tmp.unlink()
        raise ValueError(f"Could not set the tags of {path.stem}; edit its frontmatter by hand.")
    tmp.replace(path)


@mcp.tool(structured_output=False)
async def rate_dish(recipe: str, rating: str | None) -> str:
    """Remember what the household thinks of a main dish or soup, and return a line to relay as it is.

    recipe: its name as the user says it. rating: "liked" (planned more often), "never" (not planned again),
    or null to forget an earlier rating.
    """
    if rating not in (None, *RATINGS):
        raise ValueError(f"rating must be liked, never or null, not {rating!r}.")
    found = await asyncio.to_thread(library)
    dish = find(found["main"] + found["soup"], recipe)
    await asyncio.to_thread(retag, LIBRARY / f"{dish['id']}.cook", rating)
    return {"liked": f"Anotado: {dish['name']} entra mais vezes nos planos.",
            "never": f"Anotado: {dish['name']} não volta a ser planeado.", None: f"Anotado: {dish['name']} volta ao normal."}[rating]


@mcp.tool(structured_output=False)
async def swap_day(day: str, protein: str | None = None, recipe: str | None = None) -> str:
    """Change the main dish of one day of the current plan and return the updated summary to relay as it is.

    day: as the user or the table says it, e.g. "quarta", "qua", "28/10" or "2026-10-28". A day of leftovers
    changes the dish they come from. protein: "meat", "fish" or "vegetarian" when the user asks for one.
    recipe: a dish the user named, by name. With neither, it picks another dish that follows the rules.
    """
    if protein not in (None, "meat", "fish", "vegetarian"):
        raise ValueError(f"protein must be meat, fish or vegetarian, not {protein!r}.")
    async with plan_lock:
        state = load()
        iso = cooked_on(state, resolve(state, day))
        mains, soups = kept(state, skip_day=iso)
        current = next(d["main"] for d in state["plan"]["days"] if d["date"] == iso)
        exclude = set(state["exclude"]) | ({current["id"]} if current else set())
        catalog = await gather(plan_span(state))
        if recipe:
            try:
                match = find(catalog[1], recipe)
            except ValueError:
                if any(plain(recipe) in plain(f.stem) for f in LIBRARY.glob("*.cook")):
                    raise ValueError(f"{recipe!r} is in the library but not planned as a main dish: give it the `mains` "
                                     "tag as formatting.md says, then call swap_day again.") from None
                raise
            mains[iso] = match["id"]
            exclude.discard(match["id"])
        state = await replan(state, mains, soups, exclude, {iso: protein} if protein else None, catalog)
    return summary(state)


@mcp.tool(structured_output=False)
async def swap_soup(day: str) -> str:
    """Change the soup that covers a day of the current plan and return the updated summary to relay as it is.

    day: any day the soup covers, e.g. "quinta", "29/10" or "2026-10-29".
    """
    async with plan_lock:
        state = load()
        iso = resolve(state, day)
        block = next(b for b in state["plan"]["soups"] if b["start"] <= iso <= b["end"])
        mains, soups = kept(state, skip_soup=block["start"])
        exclude = set(state["exclude"]) | ({block["soup"]["id"]} if block["soup"] else set())
        state = await replan(state, mains, soups, exclude)
    return summary(state)


def groceries(state):
    """What the current plan sends to Cookidoo's shopping list: its Cookidoo copies as recipes (with Cookidoo's own
    ingredients), and one combined free-text list for the rest. Days already in the user's My week are skipped."""
    plan = state["plan"]
    cooked = [d["main"] for d in plan["days"] if d["main"] and d["fixed"] != "cookidoo" and not d.get("leftover")]
    recipes = cooked + [b["soup"] for b in plan["soups"] if b["soup"]]
    cookidoo_recipes = [r for r in recipes if r["cookidoo_id"]]
    paths = [str(LIBRARY / f"{r['id']}.cook") for r in recipes if not r["cookidoo_id"]]
    # from the library, cook reads its config/: aisle.conf merges an ingredient's other names, pantry.conf drops staples
    out = subprocess.run([COOK, "shopping-list", "--plain", *paths], cwd=LIBRARY, capture_output=True, text=True, check=True).stdout if paths else ""
    # cook pads the name column, so two or more spaces separate the name from its quantity
    items = [(*re.split(r"\s{2,}", line.strip(), maxsplit=1), "")[:2] for line in out.splitlines() if line.strip()]
    return cookidoo_recipes, items


@mcp.tool(structured_output=False)
async def groceries_preview() -> str:
    """What committing the current plan puts on Cookidoo's shopping list, as tables to show the user exactly as
    they are."""
    cookidoo_recipes, items = await asyncio.to_thread(groceries, load())
    rows = ["| Cookidoo | |", "|---|---|"]
    rows += [f"| Lista de compras | ingredientes de {link(r)} |" for r in cookidoo_recipes]
    if len(rows) == 2:
        rows.append("| - | nada |")
    rows += ["", "| Artigo | Quantidade |", "|---|---|", *(f"| {name} | {qty} |" for name, qty in items)]
    return "\n".join(rows)


@mcp.tool(structured_output=False)
async def sync_cookidoo() -> str:
    """Copy the household's Cookidoo collection into the recipe library, so new Cookidoo recipes can be planned.
    Cookidoo itself is only read. Safe to run any time; it also runs daily."""
    return "\n".join(["Cookidoo sincronizado:", *(f"- {line}" for line in await cookidoo("sync"))])


async def preview(start, days):
    first = date.fromisoformat(start) if start else date.today()
    span = [first + timedelta(n) for n in range(days)]
    soups, mains, planned, notes, recently = await gather(span)
    plan = compose(span, soups, mains, planned, {}, {}, set(), None, recently)
    plan["notes"][:0] = notes
    return summary({"start": first.isoformat(), "plan": plan})


def main():
    p = argparse.ArgumentParser(prog="meals", description=__doc__.splitlines()[0])
    sub = p.add_subparsers(dest="command", required=True)
    sub.add_parser("serve", help="the MCP server, over stdio")
    c = sub.add_parser("propose", help="print a fresh plan, without touching the agent's current one")
    c.add_argument("--start")
    c.add_argument("--days", type=int, default=7)
    args = p.parse_args()
    match args.command:
        case "serve":
            mcp.run()
        case "propose":
            print(asyncio.run(preview(args.start, args.days)))


if __name__ == "__main__":
    main()
