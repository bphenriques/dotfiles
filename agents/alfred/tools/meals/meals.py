"""The household meal planner: an MCP server over the Cooklang recipe library that applies the household rules,
keeps the current plan, and hands groceries to the `cookidoo` program."""
import argparse
import asyncio
import difflib
import json
import random
import re
import subprocess
import unicodedata
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Literal, TypedDict
from urllib.parse import quote

from mcp.server.mcpserver import MCPServer
from mcp.server.mcpserver.exceptions import ToolError

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
COURSES = {"Pratos": "main", "Sopas": "soup"}   # the library folders the planner plans from
RATINGS = {"liked": "favorita", "never": "nunca"}   # each rating's tag, which the Cookidoo sync keeps on its copies
GATHERING = "grupos"   # a tag for dishes meant for guests, planned only when the user names one
MEAL_SERVINGS = 2   # one per adult; a main lasts as many days as its servings cover
LEFTOVER_DAYS = 3   # days after cooking that leftovers are still eaten
LEVELS = ("easy", "medium", "hard")
SHOWN_FOR = timedelta(hours=1)   # a "sim" in a later chat must not save a plan the user no longer has in front of them


def words(*terms):
    return re.compile(r"\b(?:" + "|".join(map(re.escape, terms)) + r")(?:s|es)?\b", re.IGNORECASE)


MEAT_RE, FISH_RE = words(*MEAT), words(*FISH)

mcp = MCPServer("meals")
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
    for folder, course in COURSES.items():
        for f in sorted((LIBRARY / folder).glob("*.cook")):
            ident = f"{folder}/{f.stem}"   # as plans link it: @./Pratos/Arroz de pato
            try:
                recipe = read(f)
            except subprocess.CalledProcessError:
                found["broken"].append(ident)
                continue
            meta = recipe["metadata"]["map"]
            tags = meta.get("tags") or []
            found[course].append({
                "source": "cookidoo" if meta.get("cookidoo") else "library", "id": ident, "cookidoo_id": meta.get("cookidoo"),
                "name": f.stem, "url": f"{SITE}/recipe/{quote(ident)}.html",
                "protein": protein([i["name"] for i in recipe["ingredients"]], meta.get("protein")),
                "minutes": meta.get("time"), "difficulty": meta.get("difficulty"), "servings": meta.get("servings"),
                "rating": next((r for r, t in RATINGS.items() if t in tags), None), "gathering": GATHERING in tags,
                "ingredients": [i["name"] for i in recipe["ingredients"]],
            })
    return found


async def cookidoo(*args, order=None):
    """Run the cookidoo program and return its JSON; its one-line error becomes the exception."""
    try:
        done = await asyncio.to_thread(subprocess.run, [COOKIDOO, *args], input=order and json.dumps(order),
                                       capture_output=True, text=True, timeout=COOKIDOO_TIMEOUT)
    except subprocess.TimeoutExpired:
        raise ToolError(f"cookidoo {args[0]}: sem resposta do Cookidoo em {COOKIDOO_TIMEOUT} s") from None
    if done.returncode:
        raise ToolError(done.stderr.strip().splitlines()[-1] if done.stderr.strip() else f"cookidoo {args[0]} failed")
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
    except ToolError as e:
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


def within(r, limit):
    """Whether a main fits a limit {"max_minutes": n, "difficulty": the hardest allowed}; a missing field never fits."""
    limit = limit or {}
    if limit.get("max_minutes") and not (r["minutes"] and r["minutes"] <= limit["max_minutes"]):
        return False
    return not limit.get("difficulty") or (r["difficulty"] in LEVELS and LEVELS.index(r["difficulty"]) <= LEVELS.index(limit["difficulty"]))


def described(limit):
    parts = ([f"até {limit['max_minutes']} min"] if limit.get("max_minutes") else []) + \
        ([{"easy": "fácil", "medium": "fácil ou média", "hard": "qualquer dificuldade"}[limit["difficulty"]]] if limit.get("difficulty") else [])
    return ", ".join(parts)


def check(max_minutes, difficulty):
    """The limit a tool was asked for, or None."""
    if difficulty not in (None, *LEVELS):
        raise ToolError(f"difficulty must be easy, medium or hard, not {difficulty!r}.")
    if max_minutes is not None and max_minutes <= 0:
        raise ToolError("max_minutes must be a positive number of minutes.")
    return {k: v for k, v in {"max_minutes": max_minutes, "difficulty": difficulty}.items() if v} or None


def suits(r, wish):
    """Whether a main fits what a day's wish asks of it: a protein, a time limit, one of the dishes it allows."""
    return r["protein"] == wish.get("protein", r["protein"]) and within(r, wish.get("limit")) and r["id"] in wish.get("only", [r["id"]])


def compose(span, soups, mains, planned, keep_mains, keep_soups, exclude, recent=frozenset(), wishes=None):
    """A plan that keeps the fixed days and fills the rest by the household rules, each main followed by its leftovers
    while its portions last. Seeded by the start day, so the same inputs give the same plan and a change touches only
    what it names. `wishes` is {date: wish} (see wish_of): who eats that day, or a note that it needs no dinner, and
    what its main must be, which wins over the rules. Dishes in `recent` (the last two weeks) come last among those
    that fit."""
    wishes = wishes or {}
    people = [wishes.get(d.isoformat(), {}).get("people", MEAL_SERVINGS) for d in span]
    skip = [wishes.get(d.isoformat(), {}).get("skip") for d in span]
    rng = random.Random(span[0].isoformat())
    by_id = {r["id"]: r for r in soups + mains}
    pool = [r for r in mains if r["id"] not in exclude and r.get("rating") != "never" and not r.get("gathering")]
    soup_pool = [r for r in soups if r["id"] not in exclude and r.get("rating") != "never" and not r.get("gathering")]

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
        """The days dish r feeds if cooked on day i: that day, then, while its portions last, the next free days eaten
        at home (a day out keeps them for the one after), for at most LEFTOVER_DAYS."""
        days, left, j = [i], (r["servings"] or people[i]) - people[i], i + 1
        while j < len(span) and j - i <= LEFTOVER_DAYS and (skip[j] or chosen[j] is None and left >= people[j]):
            if not skip[j]:
                days.append(j)
                left -= people[j]
            j += 1
        return days

    def place(i, r):
        days = cover(i, r)
        for j in days:
            chosen[j], leftover[j] = r, j > i
        used.add(r["id"])
        return days

    def unplace(days):
        used.discard(chosen[days[0]]["id"])
        for j in days:
            chosen[j], leftover[j] = None, False

    def eaten(start, step):
        """The dish of the nearest day with dinner from `start`, going by `step`."""
        j = start
        while 0 <= j < len(span) and skip[j]:
            j += step
        return chosen[j] if 0 <= j < len(span) else None

    def options(i):
        """Unused dishes for day i that fit its wish, and those of them that follow the rules, the wanted protein first."""
        wish = wishes.get(span[i].isoformat(), {})
        free = [r for r in pool if r["id"] not in used and suits(r, wish)]
        last = next((c["protein"] for c in reversed(chosen[:i]) if c and c["protein"] in ("meat", "fish")), "fish")
        want = wish.get("protein") or ("fish" if last == "meat" else "meat")
        fits = [r for r in free if (r["servings"] or MEAL_SERVINGS) >= people[i]]
        fits = [r for r in fits if not clash(r, eaten(i - 1, -1)) and not clash(r, eaten(cover(i, r)[-1] + 1, 1))]
        return sorted(fits, key=lambda r: (r["protein"] != want, *rank(r))), free, want

    def fill(i):
        """Backtracking: a greedy pass can corner itself (hard, then hard or same protein) when a valid plan exists."""
        if i == len(span):
            return True
        if chosen[i] or skip[i]:
            return fill(i + 1)
        for r in options(i)[0][:6]:
            days = place(i, r)
            if fill(days[-1] + 1):
                return True
            unplace(days)
        return False

    for i in range(len(span)):   # a fixed day's leftovers come first, so the dishes around them are chosen to fit
        if fixed[i]:
            place(i, chosen[i])

    if not fill(0):   # no plan follows every rule: fill in order and say where it breaks
        for i, d in enumerate(span):
            if chosen[i] or skip[i]:
                continue
            fits, free, want = options(i)
            pick = next(iter(fits), None) or next(iter(free), None)
            if pick is None:
                limit = described(wishes.get(d.isoformat(), {}).get("limit") or {})
                notes.append(f"{label(d)}: não há mais pratos " + (f"({limit}) por usar." if limit else f"de {PROTEINS.get(want, want)} por usar."))
                continue
            if (pick["servings"] or MEAL_SERVINGS) < people[i]:
                notes.append(f"{label(d)}: nenhum prato serve {people[i]}; {pick['name']} serve {pick['servings'] or MEAL_SERVINGS}.")
            elif not fits:
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
    days = [{"date": d.isoformat(), "main": chosen[i], "fixed": fixed[i], "leftover": leftover[i], "people": people[i], "note": skip[i]}
            for i, d in enumerate(span)]
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
                 for r in dishes]
        items = [f"- {d['note']}"] * bool(d.get("note")) + items or ["- por decidir"]
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
    """What Alfred says: a table of the dish names per day, then the plan's page once it is committed (the links
    live there), or the question whether to commit it. A table, because the model redraws plain lines as one and
    fills its cells by guessing."""
    plan, lines = state["plan"], ["| Dia | Jantar | Sopa |", "|---|---|---|"]
    for d in plan["days"]:
        main = d["main"]
        block = next(b for b in plan["soups"] if b["start"] <= d["date"] <= b["end"])
        dish = d.get("note") or ("por decidir" if not main else f"sobras de {main['name']}" if d.get("leftover") else main["name"])
        if main and not d.get("leftover") and d.get("people", MEAL_SERVINGS) != MEAL_SERVINGS:
            dish += f" ({d['people']} pessoas)"
        soup = "por decidir" if not block["soup"] else block["soup"]["name"] if block["start"] == d["date"] else "sobras"
        lines.append(f"| {label(d['date'])} | {dish} | {soup} |")
    lines.append("")
    if state.get("limit"):
        lines.append(f"Pratos: {described(state['limit'])}.")
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
        except ToolError as e:
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
    except ToolError as e:
        notes.append(f"A lista de compras não foi atualizada ({e}); guarda de novo para tentar outra vez.")
    save(state)
    return state, notes


@mcp.tool(structured_output=False)
async def commit_plan() -> str:
    """Save the current plan to the recipe library, where the recipes site shows it with links, put its Cookidoo
    dishes in Cookidoo's My week and its groceries on Cookidoo's shopping list, and return the summary to relay as it
    is. Only when the user says to save or confirm it; a later change to the plan or its recipes needs saving again."""
    async with plan_lock:
        state = load()
        if not shown_lately(state):
            raise ToolError("This plan was not shown in the last hour: call plan_week, relay it, and save only "
                            "when the user confirms it.")
        state, notes = await commit(state)
    return "\n".join([summary(state), *notes])


def load():
    if not PLAN.exists():
        raise ToolError("There is no plan yet: call plan_week first.")
    return json.loads(PLAN.read_text())


def shown_lately(state):
    return datetime.now() - datetime.fromisoformat(state.get("shown", "2000-01-01")) <= SHOWN_FOR


def save(state):
    """Every save is followed by showing the plan, so it also records when it was last shown."""
    state["shown"] = datetime.now().isoformat(timespec="seconds")
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
    """A day of the current plan from what the user or the table says: 2026-10-28, 28/10, qua, quarta, 4f (4ª feira)
    or a dish on it."""
    dates = [d["date"] for d in state["plan"]["days"]]
    text = plain(day)
    nth = re.match(r"([2-6])\s*[af]", text)   # segunda-feira is the 2nd day, so 2f is Monday
    for iso in dates:
        d = date.fromisoformat(iso)
        named = text[:3] == plain(WEEKDAYS[d.weekday()]) or nth and int(nth[1]) - 2 == d.weekday()
        if text in (iso, f"{d:%d/%m}", f"{d.day}/{d.month}") or named:
            return iso
    for d in state["plan"]["days"]:
        if d["main"] and text in plain(d["main"]["name"]):
            return d["date"]
    raise ToolError(f"{day!r} is neither a day nor a dish of the current plan ({', '.join(map(label, dates))}).")


def cooked_on(state, iso):
    """The day the dish eaten on `iso` is cooked: that day, or the one whose leftovers it has."""
    days = state["plan"]["days"]
    i = next(n for n, d in enumerate(days) if d["date"] == iso)
    while i and days[i].get("leftover"):
        i -= 1
    return days[i]["date"]


class Wish(TypedDict, total=False):
    day: str
    people: int
    away: bool
    note: str
    recipe: str
    ingredient: str
    protein: Literal["meat", "fish", "vegetarian"]
    max_minutes: int
    difficulty: Literal["easy", "medium", "hard"]
    swap: bool


DISH = ("recipe", "ingredient", "protein", "max_minutes", "difficulty")   # what a wish asks of a day's main


def merged(old, change):
    """A day's wish after a change: eating out or a note replaces what was asked of its dinner, and the reverse."""
    new = old | {k: v for k, v in change.items() if k not in ("day", "swap")}
    if change.get("away") or change.get("note"):
        new = {k: v for k, v in new.items() if k not in DISH}
    elif any(k in change for k in DISH) or change.get("away") is False:
        new = {k: v for k, v in new.items() if k not in ("away", "note")}
    return new


def dish(mains, name):
    """The main dish the user named."""
    try:
        return find(mains, name)
    except ToolError:
        if any(plain(name) in plain(f.stem) for f in LIBRARY.rglob("*.cook")):
            raise ToolError(f"{name!r} is in the library but not planned as a main dish: the user moves it into "
                            "Pratos/, then ask again.") from None
        raise


def wish_of(stored, mains, words, limit=None):
    """A day's stored wish as compose takes it: who eats, whether it needs no dinner, and what its main must be."""
    wish = {"people": stored.get("people", MEAL_SERVINGS), "skip": "fora" if stored.get("away") else stored.get("note")}
    if stored.get("protein"):
        wish["protein"] = stored["protein"]
    if day_limit := {**(limit or {}), **(check(stored.get("max_minutes"), stored.get("difficulty")) or {})}:
        wish["limit"] = day_limit
    if food := stored.get("ingredient"):
        only = [r["id"] for r in mains if holds(r, food, words)]
        if not only and (fixed := corrected(food, words, mains)):
            only = [r["id"] for r in mains if holds(r, fixed, words)]
        if not only:
            raise ToolError(f"No main dish with {food!r} in the library.")
        wish["only"] = only
    return wish


async def replan(state, keep_mains, keep_soups, exclude, catalog=None):
    """Compose and save a new plan for the state's days, with each day's wish and the week's limit on every day."""
    span = plan_span(state)
    soups, mains, planned, notes, recently = catalog or await gather(span)
    stored, words = state.get("wishes", {}), dictionary()
    wishes = {d.isoformat(): wish_of(stored.get(d.isoformat(), {}), mains, words, state.get("limit")) for d in span}
    keep = keep_mains | {iso: dish(mains, w["recipe"])["id"] for iso, w in stored.items() if w.get("recipe")}
    keep = {iso: r for iso, r in keep.items() if not wishes.get(iso, {}).get("skip")}
    plan = compose(span, soups, mains, planned, keep, keep_soups, set(exclude), recently, wishes)
    plan["notes"][:0] = notes
    state = {**state, "exclude": sorted(exclude), "plan": plan, "committed": False}
    save(state)
    return state


def kept(state, skip_soup=None):
    """The current plan's cooked mains and its soups, by date: what a change keeps unless it frees their day."""
    plan = state["plan"]
    mains = {d["date"]: d["main"]["id"] for d in plan["days"] if d["main"] and not d.get("leftover")}
    soups = {b["start"]: b["soup"]["id"] for b in plan["soups"] if b["soup"] and b["start"] != skip_soup}
    return mains, soups


@mcp.tool(structured_output=False)   # plain text for the user; a structured copy would only double it
async def plan_week(start: str | None = None, days: int = 7, wishes: list[Wish] | None = None, different: bool = False) -> str:
    """The week's meal plan, as a table to relay as it is. Without wishes, shows the current plan if there is one for
    these days, saved or shown in the last hour, and makes one otherwise; with wishes, or different, makes a new one.
    To change the plan afterwards, use adjust_week.

    start: first day, YYYY-MM-DD (default: the current plan's while today is in it, otherwise today).
    days: how many days (default 7).
    wishes: what the user wants, with adjust_week's fields, each for a `day` ("quinta", "29/10"), or with no day for the whole week: a limit then
    holds every day, and a recipe or food goes on the first free day ("esta semana sem tempo, e algo com salmão").
    different: true for other options: a new plan that avoids every dish of the current one, keeping its wishes.
    """
    async with plan_lock:
        current = load() if PLAN.exists() else None
        if start:
            first = date.fromisoformat(start)
        elif current and date.today() in plan_span(current):   # mid-week, "the plan" is the one being cooked
            first, days = date.fromisoformat(current["start"]), current["days"]
        else:
            first = date.today()
        same = current and (current["start"], current["days"]) == (first.isoformat(), days)
        if same and not wishes and not different and (current.get("committed") or shown_lately(current)):   # else a stale draft
            save(current)
            return summary(current)
        span = [first + timedelta(n) for n in range(days)]
        catalog = await gather(span)
        stored, exclude, limit = {}, set(), None
        if same:   # days already eaten stay as they were
            stored = {d: w for d, w in current.get("wishes", {}).items() if date.fromisoformat(d) < date.today()}
        if different and same:
            stored, limit = current.get("wishes", {}), current.get("limit")
            exclude = set(current["exclude"]) | {d["main"]["id"] for d in current["plan"]["days"] if d["main"] and d["fixed"] != "cookidoo"}
            exclude |= {b["soup"]["id"] for b in current["plan"]["soups"] if b["soup"]}
        frame = {"plan": {"days": [{"date": d.isoformat(), "main": None} for d in span]}}
        for wish in wishes or []:
            if wish.get("day"):
                iso = resolve(frame, wish["day"])
            elif any(k in wish for k in ("recipe", "ingredient", "protein")):
                iso = next((d.isoformat() for d in span if d >= date.today() and d.isoformat() not in catalog[2].keys() | stored.keys()), None)
                if iso is None:
                    raise ToolError("No free day left this week for that wish: give it a day.")
            elif any(k in wish for k in ("people", "away", "note", "swap")):
                raise ToolError("Who eats, a dinner out or a note is about one day: give it a day.")
            else:
                limit = check(wish.get("max_minutes"), wish.get("difficulty")) or limit
                continue
            stored[iso] = merged(stored.get(iso, {}), wish)
        past = {d: r for d, r in kept(current)[0].items() if date.fromisoformat(d) < date.today()} if same else {}
        shopping = current.get("shopping") if same else None   # replaced on the next send
        state = await replan({"start": first.isoformat(), "days": days, "in_week": placed(), "shopping": shopping,
                              "limit": limit, "wishes": stored}, past, {}, exclude, catalog)
    return summary(state)


@mcp.tool(structured_output=False)
async def adjust_week(changes: list[Wish]) -> str:
    """Change the current plan as the week unfolds and return the table to relay as it is. Every dish the user has
    seen stays, except on the days a change frees; earlier wishes stay too. Call it again for each new change.

    changes: each names a `day` as the user or the table says it ("quarta", "qua", "28/10"), or the dish to replace
    ("lasanha"), and any of:
    people: how many eat that day ("temos 6 à mesa"): its dish must serve them, so it leaves fewer leftovers.
    away: true when nobody dines at home ("jantamos fora"), its leftovers then go to the next day; false undoes it.
    note: a day that needs no dish from the planner, in the user's words ("restos do almoço").
    recipe: a dish the user named. ingredient: a food they want, as they say it ("salmão", "salmon", even misspelt).
    protein: "meat", "fish" or "vegetarian". max_minutes, difficulty ("easy", "medium" or "hard", the hardest
    allowed): e.g. "algo rápido" is 30 and "easy".
    swap: true for just another dish; on a day of leftovers, it changes the dish they come from.
    Any other change is about that day itself. Days before today never change.
    """
    async with plan_lock:
        state = load()
        stored, (mains, soups), exclude = dict(state.get("wishes", {})), kept(state), set(state["exclude"])
        for change in changes:
            if not change.get("day"):
                raise ToolError("Each change needs a day: the one it is about.")
            iso = resolve(state, change["day"])
            if date.fromisoformat(iso) < date.today():
                raise ToolError(f"{label(iso)} já passou: só os dias de hoje em diante mudam.")
            day = next(d for d in state["plan"]["days"] if d["date"] == iso)
            about_dish = change.get("swap") or any(k in change for k in DISH)
            if about_dish and day["fixed"] == "cookidoo":
                raise ToolError(f"{label(iso)} is {day['main']['name']}, from Cookidoo's My week: the user changes it there.")
            if change.get("swap"):   # another dish: on a day of leftovers, the one they come from
                iso = cooked_on(state, iso)
                if current := mains.pop(iso, None):
                    exclude.add(current)
            elif about_dish:
                mains.pop(iso, None)
            else:   # who eats changed: its dish stays only if it still serves them
                serves = day["main"] and not day["leftover"] and (day["main"]["servings"] or MEAL_SERVINGS) >= change.get("people", day.get("people", MEAL_SERVINGS))
                if not serves or change.get("away") or change.get("note"):
                    mains.pop(iso, None)
            stored[iso] = merged(stored.get(iso, {}), change)
        state = await replan({**state, "wishes": stored}, mains, soups, exclude)
    return summary(state)


def find(dishes, name):
    """The one dish named `name`, or else the one whose name contains it."""
    matches = [r for r in dishes if plain(r["name"]) == plain(name)] or [r for r in dishes if plain(name) in plain(r["name"])]
    if len(matches) != 1:
        raise ToolError(f"{len(matches)} dishes match {name!r}: {[r['name'] for r in matches[:6]]}")
    return matches[0]


def dictionary():
    """config/aisle.conf as {name: its line's first name}, accents and case aside: the library's one list of food names,
    plurals and English included, so salmon and salmões both mean salmão."""
    conf = LIBRARY / "config" / "aisle.conf"
    lines = conf.read_text().splitlines() if conf.exists() else []
    return {plain(n): plain(names[0]) for names in (line.split("|") for line in lines if line.strip() and not line.startswith("["))
            for n in names}


def holds(dish, query, words):
    """Whether a dish's name or ingredients name every word of `query`, as whole words (sal is not salmão), each also as
    the food the dictionary takes it for."""
    names = [dish["name"], *dish["ingredients"], *(words.get(plain(i), "") for i in dish["ingredients"])]
    for term in map(plain, query.split()):
        wanted = {term, words.get(term, term), words.get(re.sub(r"(o|a)es$", "ao", term), term), words.get(term.removesuffix("s"), term)}
        if not any(re.search(rf"\b{re.escape(w)}(s|es)?\b", plain(n)) for w in wanted for n in names if n):
            return False
    return True


def corrected(query, words, dishes):
    """`query` with each word that names nothing replaced by the closest word that does, or None when a word has none
    close (then it is a food the library lacks, not a typo)."""
    vocabulary = {w: w for name in words for w in name.split()}
    vocabulary |= {plain(w): w for d in dishes for n in [d["name"], *d["ingredients"]] for w in re.findall(r"\w[\w-]*", n)}
    fixed = []
    for term in query.split():
        t = plain(term)
        if {t, t.removesuffix("s"), re.sub(r"(o|a)es$", "ao", t)} & vocabulary.keys():
            fixed.append(term)
        elif close := difflib.get_close_matches(t, vocabulary, n=1, cutoff=0.8):
            fixed.append(vocabulary[close[0]])
        else:
            return None
    return " ".join(fixed) if fixed != query.split() else None


def retag(path, rating):
    """Set a recipe's rating tag (None clears it), checked through cook so a frontmatter it cannot rewrite is left as is."""
    tags = read(path)["metadata"]["map"].get("tags") or []
    want = [t for t in tags if t not in RATINGS.values()] + ([RATINGS[rating]] if rating else [])
    text = path.read_text()
    new = re.sub(r"(?m)^tags:.*$", lambda _: f"tags: {json.dumps(want, ensure_ascii=False)}", text, count=1)
    tmp = path.with_name(f".{path.name}.tmp")
    tmp.write_text(new)
    if read(tmp)["metadata"]["map"].get("tags") != want:
        tmp.unlink()
        raise ToolError(f"Could not set the tags of {path.stem}; edit its frontmatter by hand.")
    tmp.replace(path)


@mcp.tool(structured_output=False)
async def rate_dish(recipe: str, rating: str | None) -> str:
    """Remember what the household thinks of a main dish or soup, and return a line to relay as it is.

    recipe: its name as the user says it. rating: "liked" (planned more often), "never" (not planned again),
    or null to forget an earlier rating.
    """
    if rating not in (None, *RATINGS):
        raise ToolError(f"rating must be liked, never or null, not {rating!r}.")
    found = await asyncio.to_thread(library)
    dish = find(found["main"] + found["soup"], recipe)
    await asyncio.to_thread(retag, LIBRARY / f"{dish['id']}.cook", rating)
    return {"liked": f"Anotado: {dish['name']} entra mais vezes nos planos.",
            "never": f"Anotado: {dish['name']} não volta a ser planeado.", None: f"Anotado: {dish['name']} volta ao normal."}[rating]


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
async def find_recipes(words: str, folder: str | None = None) -> str:
    """The library's recipes whose name or ingredients hold every word given: a food in any form or language
    config/aisle.conf knows (salmon, salmões and salmão alike), or a word of the name. Pass the user's words as they
    said them: a misspelt word is corrected to the closest one, and the reply says so. Returns their paths, for
    read_recipe, those named so first.

    folder: one folder to look in, e.g. "Sopas"."""
    def scan():
        dishes = []
        for f in sorted(LIBRARY.glob(f"{folder or '*'}/*.cook")):
            try:
                dishes.append({"name": f.stem, "ingredients": [i["name"] for i in read(f)["ingredients"]], "path": f"{f.parent.name}/{f.stem}"})
            except subprocess.CalledProcessError:
                continue
        return dishes

    def matching(query):
        hits = [(not holds({**d, "ingredients": []}, query, known), d["path"]) for d in dishes if holds(d, query, known)]
        return [path for _, path in sorted(hits)]
    known, dishes = dictionary(), await asyncio.to_thread(scan)
    found, note = matching(words), ""
    if not found and (fixed := corrected(words, known, dishes)):
        found, note = matching(fixed), f"Sem {words!r}; com {fixed!r}: "
    if not found:
        return f"Nenhuma receita com {words!r}" + (f" em {folder}." if folder else ".")
    return "\n".join([f"{note}{len(found)} receita(s):", *found[:40], *(["…"] if len(found) > 40 else [])])


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
    plan = compose(span, soups, mains, planned, {}, {}, set(), recently)
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
