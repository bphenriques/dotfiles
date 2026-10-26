"""The only program that talks to Cookidoo: sign-in, copying the collection into the recipe library, reading and
changing My week, and sending groceries. Commands print JSON for `meals`; Cookidoo is written only by `plan` and `send`."""
import argparse
import asyncio
import contextlib
import fcntl
import getpass
import html
import json
import os
import re
import subprocess
import sys
from datetime import date, timedelta
from pathlib import Path

import aiohttp
from cookidoo_api import Cookidoo
from cookidoo_api.helpers import get_localization_options
from cookidoo_api.types import CookidooConfig

TOKEN = Path("@tokenFile@")
LIBRARY = Path("@library@")
COLLECTION = "@collection@"
COOK = "@cook@"
COUNTRY, LANGUAGE = "pt", "pt-PT"
LOCALIZATION = None  # set in main(); it reads a bundled file, but through an async API

# The library's category tags and the `course` each implies, as formatting.md defines them.
CATEGORIES = {"mains": "main", "soups": "starter", "desserts": "dessert", "breakfast": "breakfast", "baby-food": None}
SYNC_TAG = "cookidoo"   # marks a copy of a Cookidoo recipe; its `cookidoo:` field is its identity
PROSE = str.maketrans({"@": "a", "#": "nº", "~": "-", "{": "(", "}": ")"})
MAX_IMAGE = 10 * 2**20


def words(*terms):
    return re.compile(r"\b(?:" + "|".join(map(re.escape, terms)) + r")(?:s|es)?\b", re.IGNORECASE)


SOUP_RE = words("sopa", "creme", "caldo", "soup")
DESSERT_RE = words("bolo", "bolacha", "biscoito", "sobremesa", "pudim", "mousse", "gelado", "cake", "dessert")


# Cookidoo adapter. The only code that knows cookidoo-api: everything below it works on the plain data these return,
# so a change in Cookidoo or in the library is a change here, and test_cookidoo's contract test catches the latter.

@contextlib.asynccontextmanager
async def session(login=None):
    """A Cookidoo session: every refresh rotates the token, so two must never overlap.
    Without a login config it resumes from the saved token."""
    if login is None and not TOKEN.exists():
        raise RuntimeError("Not signed in to Cookidoo. The user has to run `sudo -u hermes cookidoo login` once.")
    with open(f"{TOKEN}.lock", "a") as held:
        fcntl.flock(held, fcntl.LOCK_EX)
        async with aiohttp.ClientSession(cookie_jar=aiohttp.CookieJar(unsafe=True)) as http:
            api = Cookidoo(http, cfg=login or CookidooConfig(localization=LOCALIZATION))
            api.on_auth_data_update = lambda _: api.save_token(TOKEN)
            if login is None:
                api.load_token(TOKEN)
            yield api


def localization():
    return asyncio.run(get_localization_options(country=COUNTRY, language=LANGUAGE))[0]


def recipe_of(d):
    """A Cookidoo recipe as plain data."""
    return {
        "id": d.id, "name": d.name, "url": d.url, "image": d.image, "servings": d.serving_size,
        "minutes": round(d.total_time / 60), "difficulty": d.difficulty, "categories": [c.name for c in d.categories],
        "ingredients": [{"name": i.name, "amount": i.description} for i in d.ingredients],
        "steps": [{"title": g.title, "texts": [s.formatted_text for s in g.recipe_steps]} for g in d.step_groups],
        "notes": list(d.notes), "utensils": list(d.utensils),
    }


def day_of(day):
    """A My week day as plain data: its date and the recipes on it."""
    return day.id, [{"id": r.id, "name": r.name, "url": r.url} for r in day.recipes]


async def collection_recipes(api):
    """The recipes of the household's collection, in full."""
    for count, fetch in ((api.count_custom_collections, api.get_custom_collections),
                         (api.count_managed_collections, api.get_managed_collections)):
        _, pages = await count()
        for page in range(pages):
            for c in await fetch(page):
                if c.name.casefold() == COLLECTION.casefold():
                    return [recipe_of(await api.get_recipe_details(r.id)) for chapter in c.chapters for r in chapter.recipes]
    raise RuntimeError(f"No Cookidoo collection named {COLLECTION!r}.")


async def my_week(api, span):
    """{date: [recipe, ...]} for each day of the span that has any."""
    wanted, found = {d.isoformat() for d in span}, {}
    for anchor in [*span[::7], span[-1]]:
        for day, recipes in map(day_of, await api.get_recipes_in_calendar_week(anchor)):
            if day in wanted and recipes:
                found[day] = recipes
    return found


async def schedule(api, add, remove):
    """My week: [date, id] pairs to take out, then to put in."""
    for day, recipe_id in remove:
        await api.remove_recipe_from_calendar(date.fromisoformat(day), recipe_id)
    for day, recipe_id in add:
        await api.add_recipes_to_calendar(date.fromisoformat(day), [recipe_id])


def item_of(i):
    """A shopping list free-text item as plain data: (id, text, ticked)."""
    return i.id, i.name, i.is_owned


async def additional_items(api):
    """The shopping list's free-text items: {id: (text, ticked)}."""
    return {id: (text, ticked) for id, text, ticked in map(item_of, await api.get_additional_items())}


async def shop(api, recipes, items):
    """The shopping list: Cookidoo recipes with their own ingredients, then free-text items. Returns the items' ids."""
    if recipes:
        await api.add_ingredient_items_for_recipes(recipes)
    return [item_of(i)[0] for i in await api.add_additional_items(items)] if items else []


async def unshop(api, recipes, item_ids):
    if recipes:
        await api.remove_ingredient_items_for_recipes(recipes)
    if item_ids:
        await api.remove_additional_items(item_ids)


async def sign_in(email, password):
    cfg = CookidooConfig(localization=LOCALIZATION, email=email, password=password)
    async with session(login=cfg) as api:
        await api.login()


# The copy into the library: plain recipe data in, Cooklang files out.

def read(path=None, text=None):
    """A recipe through cook's own parser, from a file or from text."""
    args = [COOK, "recipe", "read", "--format", "json", *([str(path)] if path else [])]
    return json.loads(subprocess.run(args, input=text, capture_output=True, text=True, check=True).stdout)


def clean(text):
    """Cookidoo's marked-up text as one line of plain Cooklang prose: no markup, and nothing Cooklang would parse
    (an ingredient, cookware, timer or comment inside, a section or note at the start)."""
    text = " ".join(html.unescape(re.sub(r"<[^>]+>", " ", text or "")).split())
    text = text.translate(PROSE).replace("--", "-").replace("[-", "[").replace("-]", "]")
    return text.lstrip("=> ")


def ingredient_name(i):
    return clean(i["name"]).replace("%", "")


def ingredient(i):
    """Quantities such as 100 g, 3 or "1.5 c. chá de" go in the braces; anything else (q.b., a range) stays as a note."""
    name, amount = ingredient_name(i), clean(i["amount"]).replace("%", "")
    m = re.fullmatch(r"(\d+(?:[.,]\d+)?)(?:\s*([^\W\d].*?))?(?:\s+de)?", amount)
    if not m:
        return f"@{name}{{}}" + (f"({amount})" if amount else "")
    quantity, unit = m.group(1).replace(",", "."), m.group(2)
    return f"@{name}{{{quantity}%{unit}}}" if unit else f"@{name}{{{quantity}}}"


def to_cook(r, name, tags, image):
    """A Cookidoo recipe as a Cooklang file in the library's layout (formatting.md)."""
    category = next((t for t in tags if t in CATEGORIES), "mains")
    front = {"servings": r["servings"], "time": r["minutes"], "difficulty": r["difficulty"], "course": CATEGORIES[category],
             "source": r["url"], "cookidoo": r["id"], "image": f"{name}.jpg" if image else None, "tags": tags}
    lines = ["---", *(f"{k}: {json.dumps(v, ensure_ascii=False)}" for k, v in front.items() if v is not None), "---", ""]
    lines += [", ".join(map(ingredient, r["ingredients"])) + ".", ""]
    for group in r["steps"]:
        if clean(group["title"]):
            lines += [f"= {clean(group['title'])} =", ""]
        lines += [p for text in group["texts"] for p in (clean(text), "")]
    lines += [f"> {clean(n)}" for n in r["notes"]]
    if r["utensils"]:
        lines.append(f"> Utensílios: {', '.join(map(clean, r['utensils']))}")
    return "\n".join(lines).rstrip() + "\n"


def write_if_changed(path, data):
    """Idempotent: an unchanged copy is not rewritten, so the site sees no change either."""
    if path.exists() and path.read_bytes() == data:
        return False
    tmp = path.with_name(f".{path.name}.tmp")
    tmp.write_bytes(data)
    tmp.replace(path)
    return True


def synced_copies():
    """The library's Cookidoo copies by Cookidoo id, with the tags each has. A file cook cannot read is not one.
    Copies made before the `cookidoo:` field are still recognised by their tag and link, and get the field."""
    found = {}
    for f in sorted(LIBRARY.glob("*.cook")):
        try:
            meta = read(f)["metadata"]["map"]
        except subprocess.CalledProcessError:
            continue
        tags = meta.get("tags") or []
        legacy = re.search(r"/recipes/recipe/[^/]+/(r\d+)", str(meta.get("source") or "")) if SYNC_TAG in tags else None
        if cid := meta.get("cookidoo") or (legacy and legacy.group(1)):
            found[cid] = (f, tags)
    return found


def apply(recipes, images):
    """Copy Cookidoo recipes into the library and report each one. A copy is found by its `cookidoo:` id, keeps the
    tags it has, and follows Cookidoo for everything else; a file of the user's with the same name is never touched."""
    synced, report = synced_copies(), []
    for r in recipes:
        if r["id"] in synced:
            path, tags = synced[r["id"]]
        else:
            path = LIBRARY / f"{clean(r['name']).replace('/', '-')}.cook"
            if path.exists():
                report.append(f"{path.stem}: já existe uma receita tua com este nome, não foi tocada")
                continue
            labels = " | ".join([r["name"], *r["categories"]])
            tags = ["desserts" if DESSERT_RE.search(labels) else "soups" if SOUP_RE.search(labels) else "mains", SYNC_TAG]
        existed = path.exists()
        image = path.with_name(f"{path.stem}.jpg")   # not with_suffix: a recipe name may contain a dot
        picture = images.get(r["id"]) if existed or not image.exists() else None   # a photo of yours is never replaced
        text = to_cook(r, path.stem, tags, picture is not None or (existed and image.exists()))
        # cook accepts almost anything, so check the conversion kept exactly the recipe's ingredients and nothing else
        parsed = read(text=text)
        if {i["name"] for i in parsed["ingredients"]} != {ingredient_name(i) for i in r["ingredients"]} or parsed["cookware"] or parsed["timers"]:
            report.append(f"{path.stem}: a conversão não ficou certa, não foi escrita")
            continue
        changed = write_if_changed(path, text.encode())
        if picture is not None:
            changed = write_if_changed(image, picture) or changed
        report.append(f"{path.stem}: {'atualizada' if changed and existed else 'nova' if changed else 'sem alterações'}")
    return report


async def body(chunks):
    """A whole download, stopping just past MAX_IMAGE. (aiohttp's read(n) returns up to n bytes, often one chunk.)"""
    data = b""
    async for chunk in chunks:
        data += chunk
        if len(data) > MAX_IMAGE:
            break
    return data


def photo(content_type, data):
    """A downloaded recipe photo, or None unless it is an image of a sane size."""
    return data if content_type.startswith("image/") and len(data) <= MAX_IMAGE else None


def restock(order, have):
    """What a send changes, given the list's free-text items `have`: (recipes to remove, items to remove, recipes to
    add, texts to add, ticked items to keep). Only what the earlier send added is replaced; a ticked item stays."""
    old = order.get("sent") or {"recipes": [], "items": []}
    ticked = [i for i in old["items"] if i in have and have[i][1]]
    return ([r for r in old["recipes"] if r not in order["recipes"]],
            [i for i in old["items"] if i in have and not have[i][1]],
            [r for r in order["recipes"] if r not in old["recipes"]],
            [t for t in order["items"] if t not in {have[i][0] for i in ticked}],
            ticked)


def held(pair, have):
    """Whether My week holds this [date, id] pair."""
    day, rid = pair
    return rid in {r["id"] for r in have.get(day, [])}


# Commands.

async def sync():
    """Read the household's Cookidoo collection (Cookidoo is never written) and copy it into the library."""
    async with session() as api:
        recipes = await collection_recipes(api)
    images = {}
    async with aiohttp.ClientSession() as http:
        for r in recipes:
            if r["image"]:
                async with http.get(r["image"]) as resp:
                    resp.raise_for_status()
                    if (data := photo(resp.content_type, await body(resp.content.iter_chunked(65536)))) is not None:
                        images[r["id"]] = data
    return apply(recipes, images)


async def week(start, days):
    """What My week holds for each day of the span: {date: [{id, name, url}, ...]}."""
    async with session() as api:
        return await my_week(api, [start + timedelta(n) for n in range(days)])


async def plan(order):
    """My week: {"add": [[date, id], ...], "remove": [...]}, Cookidoo ids. Only what is missing is added and only what
    is there is removed, so a retry changes nothing twice."""
    days = sorted(date.fromisoformat(day) for day, _ in order["add"] + order["remove"])
    async with session() as api:
        have = await my_week(api, [days[0] + timedelta(n) for n in range((days[-1] - days[0]).days + 1)]) if days else {}
        add = [p for p in order["add"] if not held(p, have)]
        remove = [p for p in order["remove"] if held(p, have)]
        await schedule(api, add, remove)
    return {"added": len(add), "removed": len(remove)}


async def send(order):
    """Groceries: {"recipes": [id, ...], "items": [text, ...], "sent": what the last send returned, or null}, Cookidoo
    ids. Returns what is now on the list from this plan: {"recipes": [id, ...], "items": [item id, ...]}."""
    async with session() as api:
        drop_recipes, drop_items, add_recipes, add_items, ticked = restock(order, await additional_items(api))
        await unshop(api, drop_recipes, drop_items)
        added = await shop(api, add_recipes, add_items)
    return {"recipes": order["recipes"], "items": ticked + added}


def main():
    p = argparse.ArgumentParser(prog="cookidoo", description=__doc__.splitlines()[0])
    sub = p.add_subparsers(dest="command", required=True)
    sub.add_parser("login", help="sign in once and keep only the token")
    sub.add_parser("sync", help="copy the collection into the recipe library")
    w = sub.add_parser("week", help="what My week holds for a span of days")
    w.add_argument("start", type=date.fromisoformat)
    w.add_argument("days", type=int)
    sub.add_parser("plan", help="change My week, read as JSON from stdin")
    sub.add_parser("send", help="send groceries, read as JSON from stdin")
    args = p.parse_args()

    global LOCALIZATION
    LOCALIZATION = localization()
    os.umask(0o077)
    try:
        match args.command:
            case "login":
                asyncio.run(sign_in(input("Cookidoo email: "), getpass.getpass("Cookidoo password: ")))
                print(f"Signed in; token at {TOKEN}")
            case "sync":
                print(json.dumps(asyncio.run(sync()), ensure_ascii=False))
            case "week":
                print(json.dumps(asyncio.run(week(args.start, args.days)), ensure_ascii=False))
            case "plan":
                print(json.dumps(asyncio.run(plan(json.load(sys.stdin)))))
            case "send":
                print(json.dumps(asyncio.run(send(json.load(sys.stdin)))))
    except Exception as e:   # one line on stderr for meals to relay, not a traceback
        sys.exit(f"cookidoo {args.command}: {e}")


if __name__ == "__main__":
    main()
