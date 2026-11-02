"""Recipe drafts, reviewed in `drafts/` until `promote` moves them into the library (the recipes AGENTS.md has the
lifecycle). Code checks every number the model gives against the source."""
import argparse
import html as markup
import json
import re
import shutil
import subprocess
import sys
import tempfile
import urllib.request
from fractions import Fraction
from pathlib import Path
from urllib.parse import quote, urlparse

from recipe_scrapers import scrape_html

LIBRARY = Path("@library@")
DRAFTS = Path("@drafts@")
PLAN = Path("@planFile@")
MODEL_URL = "@modelUrl@"
MODEL = "@model@"
SPEECH_URL = "@speechUrl@"
YT_DLP = "@ytDlp@"
CURL = "@curl@"
MAGICK = "@magick@"
COOK = "@cook@"
TIMEOUT = 600   # seconds, for the model and the downloads
MAX_IMAGE = 10 * 2**20
MAX_PAGE = 30000   # characters of a page's text the model reads

VIDEO_HOSTS = ("instagram.com", "youtube.com", "youtu.be", "tiktok.com")
CATEGORIES = ("Pratos", "Sopas", "Sobremesas", "Pequeno-almoço", "Molhos", "Acompanhamentos", "Bases", "Bebidas", "Bebé")   # the library's folders (formatting.md)
LEVELS = ("easy", "medium", "hard")
UA = {"User-Agent": "Mozilla/5.0"}
AISLE = "aisle.conf"
REQUIRED = ("servings", "time", "difficulty")   # formatting.md's
FIELDS = (*REQUIRED, "time.prep", "time.cook", "description", "source", "author", "tags", "cookidoo")
CHECK = "> Check:"   # a doubt in the draft, which the user deletes once resolved
# Brazilian words the model slips into European Portuguese, with the Portuguese for each.
BRAZILIAN = {"assadeira": "tabuleiro", "barbante": "cordel", "gergelim": "sésamo", "saquê": "saqué", "talharim": "massa",
             "talharins": "massa", "café da manhã": "pequeno-almoço", "creme de leite": "natas", "geladeira": "frigorífico",
             "xícara": "chávena", "suco": "sumo", "abacaxi": "ananás", "enquanto isso": "entretanto",
             "liquidificador": "liquidificadora", "cebolinha verde": "cebolinho"}

# Spoken and written numbers a quantity may come from, Portuguese and English.
NUMBER_WORDS = {"um": 1, "uma": 1, "dois": 2, "duas": 2, "três": 3, "quatro": 4, "cinco": 5, "seis": 6, "sete": 7,
                "oito": 8, "nove": 9, "dez": 10, "meio": 0.5, "meia": 0.5, "one": 1, "two": 2, "three": 3, "four": 4,
                "five": 5, "six": 6, "seven": 7, "eight": 8, "nine": 9, "ten": 10, "half": 0.5}
# One Portuguese spelling per unit (formatting.md), so the shopping list sums them; a plain count has none.
UNITS = {"gram": "g", "grams": "g", "gramas": "g", "dente": "dentes", "clove": "dentes", "cloves": "dentes",
         "folha": "folhas", "leaf": "folhas", "leaves": "folhas", "fatia": "fatias", "tira": "tiras", "talo": "talos",
         "pé": "pés", "ramo": "ramos", "lata": "latas", "gota": "gotas", "cubo": "cubos", "tbsp": "c. sopa", "tablespoon": "c. sopa",
         "tablespoons": "c. sopa", "c. de sopa": "c. sopa", "colher de sopa": "c. sopa", "colheres de sopa": "c. sopa",
         "tsp": "c. chá", "teaspoon": "c. chá", "teaspoons": "c. chá", "c. de chá": "c. chá", "colher de chá": "c. chá",
         "colheres de chá": "c. chá", "pinch": "pitada", "pinches": "pitada", "pitadas": "pitada", "bunch": "molho",
         "sprig": "raminhos", "sprigs": "raminhos", "raminho": "raminhos", "cup": "chávena", "cups": "chávena",
         "chávenas": "chávena", "unid.": "", "unid": "", "unidade": "", "unidades": "", "un.": ""}
PART = re.compile(r"(dentes?|folhas?|fatias?|tiras?|talos?) de (.+)")   # a count of parts, which goes in the unit
VULGAR = {"½": "1/2", "⅓": "1/3", "⅔": "2/3", "¼": "1/4", "¾": "3/4", "⅛": "1/8"}

INGREDIENT = {"type": "object", "required": ["name", "quantity", "unit", "note"], "properties": {
    "name": {"type": "string"}, "quantity": {"type": "string"}, "unit": {"type": "string"}, "note": {"type": "string"}}}
RULES = """Rules for each ingredient, as the source writes it (code translates it later):
- name: the product as a shop sells it, singular, lowercase, without size, state or preparation.
- quantity: a number exactly as the source gives it ("100", "1/2"), or "" when it gives none. Never estimate one.
- unit: "g", "kg", "ml", "l", "dl", "c. sopa", "c. chá", "pitada", a count of parts in the plural ("dentes",
  "cloves", "folhas"), the source's own word otherwise ("tbsp", "cup"), or "" for a plain count.
- note: size, state or preparation ("large", "picada"), or "".
Examples:
- "2 plump garlic cloves, peeled" -> garlic, 2, cloves, "plump, peeled"
- "50g unsalted butter" -> butter, 50, g, "unsalted"
- "3 large eggs" -> egg, 3, "", "large"
- "3 c. de sopa azeite" -> azeite, 3, c. sopa, ""
- "100g de cebola picada" -> cebola, 100, g, "picada"
- "sal e pimenta q.b." -> two ingredients: sal, "", "", ""; pimenta, "", "", """""


def numbers(text, words=True):
    """Every number written in `text`, as digits, fractions or (with `words`) words."""
    for k, v in VULGAR.items():
        text = text.replace(k, f" {v}")
    found = {float(Fraction(m.replace(",", "."))) for m in re.findall(r"\d+/\d+|\d+(?:[.,]\d+)?", text)}
    found |= {float(v) for w, v in NUMBER_WORDS.items() if words and re.search(rf"\b{w}\b", text, re.IGNORECASE)}
    return found


def number(quantity):
    try:
        return float(Fraction(VULGAR.get(quantity, quantity).replace(",", ".")))
    except (ValueError, ZeroDivisionError):
        return None


def ask_model(prompt, schema):
    body = {"model": MODEL, "stream": False, "think": False, "format": schema, "options": {"temperature": 0},
            "messages": [{"role": "user", "content": prompt}]}
    req = urllib.request.Request(f"{MODEL_URL}/api/chat", json.dumps(body).encode(), {"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
        return json.loads(json.loads(r.read())["message"]["content"])


def first(*getters):
    """The first getter that returns a value; scrapers raise on fields a page lacks."""
    for get in getters:
        try:
            if value := get():
                return value
        except Exception:
            pass
    return None


def from_page(url):
    req = urllib.request.Request(url, headers=UA)
    with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
        html = r.read().decode(r.headers.get_content_charset() or "utf-8", "replace")
    try:
        s = scrape_html(html, org_url=url, supported_only=False)
    except Exception:
        sys.exit("No recipe here: the page has no recipe data.")
    lines = first(s.ingredients, lambda: s.schema.ingredients()) or []
    steps = first(s.instructions_list, lambda: s.schema.instructions().split("\n")) or []
    servings = first(s.yields, lambda: s.schema.yields())
    recipe = {
        "title": first(s.title), "author": first(s.author), "image": first(s.image),
        "servings": int(m[0]) if servings and (m := re.findall(r"\d+", str(servings))) else None,
        "time": first(s.total_time), "prep": first(s.prep_time), "cook": first(s.cook_time),
        # Without the "Passo 1" labels some pages give as steps.
        "steps": [x.strip() for x in steps if x.strip() and not re.fullmatch(r"\w+ \d+", x.strip())],
        "evidence": None, "notes": [],
    }
    listed = "\n".join(f"- {line}" for line in lines)
    reply = ask_model(
        f"Recipe: {recipe['title']}\nIngredient lines:\n{listed}\n\n"
        "Give the dish's name (no site name, no \"recipe\"), its category and difficulty, and, for each line in "
        f"order, the ingredients it names (usually one).\n{RULES}",
        {"type": "object", "required": ["name", "category", "difficulty", "lines"], "properties": {
            "name": {"type": "string"}, "category": {"enum": list(CATEGORIES)}, "difficulty": {"enum": list(LEVELS)},
            "lines": {"type": "array", "items": {"type": "object", "required": ["ingredients"], "properties": {
                "ingredients": {"type": "array", "items": INGREDIENT}}}}}})
    split = reply.pop("lines")
    numbered = "\n".join(f"{i}. {x}" for i, x in enumerate(recipe["steps"], 1))
    unlisted = ask_model(
        f"Ingredient list:\n{listed}\n\nSteps:\n{numbered}\n\nWhich foods do the steps add that the ingredient list does "
        "not name at all? Not water, salt or pepper, not a food the list names in other words, not a mix of listed ones "
        "(\"the spices\", \"the sauce\"). Empty when there are none.",
        {"type": "object", "required": ["unlisted"], "properties": {"unlisted": {"type": "array", "items": {"type": "string"}}}})["unlisted"]
    if unlisted:   # a stale ingredient list: the page's text is the source, its recipe data still names the dish
        found = from_text(visible(html)[:MAX_PAGE])
        found["notes"].append(f"the page's recipe data lacks {', '.join(unlisted)}, which its steps use: drafted from the page's text")
        return found | reply | {"author": recipe["author"], "image": recipe["image"]}
    if len(split) == len(lines):
        ingredients = [x | {"line": line} for line, group in zip(lines, split) for x in group["ingredients"]]
    else:
        recipe["notes"].append(f"{len(lines)} ingredient lines came back as {len(split)}: check the list against the source")
        ingredients = [x | {"line": None} for group in split for x in group["ingredients"]]
        recipe["evidence"] = "\n".join(lines)
    return recipe | reply | {"ingredients": ingredients}


def visible(html):
    """A page's text, one line per element, without its scripts and styles."""
    text = re.sub(r"(?is)<(script|style|noscript)\b.*?</\1>|<[^>]+>", "\n", html)
    return re.sub(r"\s*\n\s*", "\n", markup.unescape(text)).strip()


def from_video(url):
    with tempfile.TemporaryDirectory() as tmp:
        subprocess.run([YT_DLP, "--quiet", "--no-warnings", "--format", "bestaudio/best", "--output", f"{tmp}/audio.%(ext)s",
                        "--print-to-file", "%(description|)s", f"{tmp}/caption.txt",
                        "--print-to-file", "%(thumbnail|)s", f"{tmp}/thumbnail.txt", url], check=True, timeout=TIMEOUT)
        caption = Path(tmp, "caption.txt").read_text().strip()
        image = Path(tmp, "thumbnail.txt").read_text().strip() or None
        audio = next(Path(tmp).glob("audio.*"))
        heard = subprocess.run([CURL, "--silent", "--show-error", "--fail", "--form", f"file=@{audio}",
                                "--form", "response_format=text", SPEECH_URL], capture_output=True, text=True, timeout=TIMEOUT)
    transcript = heard.stdout.strip() if heard.returncode == 0 else ""
    recipe = from_text(f"Caption:\n{caption or '(none)'}\n\nTranscript:\n{transcript or '(no speech)'}")
    if heard.returncode != 0:
        recipe["notes"].append("the transcription failed (the ai host may be off): only the caption was read")
    recipe["notes"].append("the steps were written by the model from the transcript")
    if image:
        recipe["notes"].append("photo: the video's cover, confirm it shows the dish or replace the .jpg")
    recipe["notes"] += [f"the caption links to {link}: a recipe page there is the better source"
                        for link in re.findall(r"https?://\S+", caption)]
    return recipe | {"image": image}


def from_text(text):
    """A recipe from free text: a video's caption and transcript, or one the user gave."""
    reply = ask_model(
        f"{text}\n\nIf this holds a recipe, give its name, category, difficulty, servings and total minutes (0 when "
        "unsaid), every ingredient, and the steps in the source's language, close to its words. Keep a word you are "
        f"unsure of as written. If there is no recipe, set recipe to false.\n{RULES}",
        {"type": "object", "required": ["recipe", "name", "category", "difficulty", "servings", "time", "ingredients", "steps"],
         "properties": {"recipe": {"type": "boolean"}, "name": {"type": "string"}, "category": {"enum": list(CATEGORIES)},
                        "difficulty": {"enum": list(LEVELS)}, "servings": {"type": "integer"}, "time": {"type": "integer"},
                        "ingredients": {"type": "array", "items": INGREDIENT},
                        "steps": {"type": "array", "items": {"type": "string"}}}})
    if not reply.pop("recipe"):
        sys.exit(f"No recipe here.\n\n{text}")
    notes = []
    for field in ("servings", "time"):
        if reply[field] and reply[field] not in numbers(text):
            notes.append(f"the source does not say {field} {reply[field]}, left out")
            reply[field] = 0
    return reply | {"ingredients": [x | {"line": None} for x in reply["ingredients"]], "title": None, "author": None,
                    "image": None, "servings": reply["servings"] or None, "time": reply["time"] or None, "prep": None,
                    "cook": None, "evidence": text, "notes": notes}


def check(recipe, names):
    """Empties quantities the source does not state, names ingredients and units in Portuguese, and returns the doubts."""
    doubts = []
    for x in recipe["ingredients"]:
        source, issues = x["line"] or recipe["evidence"], []
        if x["quantity"] and number(x["quantity"]) not in numbers(source):
            issues.append(f"the source does not say {x['quantity']} {x['unit']}".rstrip() + ", left empty")
            x["quantity"] = x["unit"] = ""
        elif not x["quantity"] and numbers(source):
            issues.append("no quantity")
        if x["name"].lower() not in source.lower():
            issues.append("not in the source as such" + (f" ({x['line']!r})" if x["line"] else ""))
        x["name"] = names.get(x["name"].lower(), x["name"].lower())
        x["unit"] = UNITS.get(x["unit"].lower(), x["unit"])
        if issues:
            doubts.append(f"{x['name']}: " + "; ".join(issues))
    for field, label in (("servings", "servings"), ("time", "total time"), ("steps", "steps"), ("image", "photo")):
        if not recipe[field]:
            doubts.append(f"no {label} in the source")
    return doubts


def translate(recipe, keep_name=False):
    """Puts the name, steps and ingredient notes in European Portuguese, by the model, and says whether that changed them."""
    before = {"name": recipe["name"], "steps": recipe["steps"], "notes": [x["note"] for x in recipe["ingredients"]]}
    reply = ask_model(
        f"{json.dumps(before, ensure_ascii=False)}\n\nGive this recipe's name, steps and ingredient notes, in order, in "
        "European Portuguese (Portugal, not Brazil), keeping every number and the meaning. Any already in Portuguese "
        "stays unchanged.",
        {"type": "object", "required": ["name", "steps", "notes"], "properties": {
            "name": {"type": "string"}, "steps": {"type": "array", "items": {"type": "string"}},
            "notes": {"type": "array", "items": {"type": "string"}}}})
    reply["steps"] = [re.sub(r"^\d+[.)]\s*", "", s) for s in reply["steps"]]   # the model numbers them at times
    if keep_name:
        reply["name"] = before["name"]
    if reply == before:
        return False
    for field in ("steps", "notes"):
        if len(reply[field]) != len(before[field]):
            recipe["notes"].append(f"the translation changed the number of {field}, so they stay as in the source")
            reply[field] = before[field]
    recipe["notes"].append("the name, steps and notes were translated by the model")
    recipe["notes"] += [f"step {i}: the translation changed its numbers"
                        for i, (a, b) in enumerate(zip(before["steps"], reply["steps"]), 1)
                        if numbers(a, words=False) != numbers(b, words=False)]   # "um" and "uma" are articles too
    for x, note in zip(recipe["ingredients"], reply.pop("notes")):
        x["note"] = note
    recipe |= reply
    return True


def ingredients(recipe):
    """The recipe's one ingredient paragraph."""
    def one(x):
        amount = f"{x['quantity']}%{x['unit']}" if x["quantity"] and x["unit"] else x["quantity"]
        note = re.sub(r"[()]", "", x["note"]).strip()
        return f"@{x['name']}{{{amount}}}" + (f"({note})" if note else "")
    return ", ".join(map(one, recipe["ingredients"])) + "."


def cooklang(recipe, doubts=()):
    """The draft's text; its photo is the .jpg beside it, which cook finds by name (an `image:` field breaks it in a folder)."""
    meta = {"servings": recipe["servings"], "time": recipe["time"], "time.prep": recipe["prep"],
            "time.cook": recipe["cook"], "difficulty": recipe["difficulty"], "source": recipe["url"],
            "author": recipe["author"]}
    front = "\n".join(f"{k}: {json.dumps(v, ensure_ascii=False)}" for k, v in meta.items() if v)
    checks = "".join(f"{CHECK} {d}\n" for d in doubts) + ("\n" if doubts else "")
    return f"---\n{front}\n---\n\n{checks}{ingredients(recipe)}\n\n" + "\n\n".join(recipe["steps"]) + "\n"


def new(url, text, name=None, category=None):
    if text:
        recipe = from_text(text)
    elif (urlparse(url).hostname or "").removeprefix("www.").endswith(VIDEO_HOSTS):
        recipe = from_video(url)
    else:
        recipe = from_page(url)
    recipe |= {"url": url} | {k: v for k, v in (("name", name), ("category", category)) if v}
    translated = translate(recipe, keep_name=bool(name))
    name = re.sub(r'[/\\:*?"<>|]', "", recipe["name"]).strip() or "Receita"
    name = name[0].upper() + name[1:]
    draft = DRAFTS / recipe["category"] / f"{name}.cook"
    if found := named(DRAFTS, name):
        sys.exit(f"A draft {found[0]} exists already: review it, or delete it to start over.")
    draft.parent.mkdir(parents=True, exist_ok=True)
    if recipe["image"]:
        try:
            if urlparse(recipe["image"]).scheme not in ("http", "https"):   # urllib would read a file:// one
                raise OSError("not a web address")
            with tempfile.NamedTemporaryFile() as f:
                with urllib.request.urlopen(urllib.request.Request(recipe["image"], headers=UA), timeout=TIMEOUT) as r:
                    data = r.read(MAX_IMAGE + 1)
                if len(data) > MAX_IMAGE:
                    raise OSError("larger than 10 MiB")
                f.write(data)
                f.flush()
                subprocess.run([MAGICK, f.name, str(draft.with_name(f"{name}.jpg"))], check=True, capture_output=True)
        except (OSError, subprocess.CalledProcessError):
            recipe["notes"].append(f"the photo could not be saved: {recipe['image']}")
            recipe["image"] = None
    names = portuguese()
    doubts = recipe["notes"] + check(recipe, names)
    if translated:
        doubts += [f"{x['name']}: no Portuguese name in config/{AISLE}" for x in recipe["ingredients"] if x["name"].lower() not in names]
    draft.write_text(cooklang(recipe, doubts))
    facts = [recipe["servings"] and f"{recipe['servings']} servings", recipe["time"] and f"{recipe['time']} min",
             recipe["difficulty"], f"photo: {recipe['image'] or 'none'}"]
    taken = " A recipe of that name is in the library already." if named(LIBRARY, name) else ""
    lines = [f"{name}: drafted in drafts/{recipe['category']}/, its folder in the library (move it to change it).{taken}",
             " · ".join(filter(None, facts)), "",
             f"```\n{ingredients(recipe)}\n```", ""]
    if doubts:
        lines += [f"{len(doubts)} to check, each a `{CHECK}` line in the draft: fix it there, then delete the line."]
        lines += [f"- {d}" for d in doubts]
    else:
        lines.append("Nothing to check.")
    print("\n".join(lines + ["", "Approve it to move it into the library."]))


def named(base, name):
    """The recipes in `base`'s folders called `name`, or `<folder>/<name>`, in any case: the share ignores it."""
    want = name.casefold()
    return [f for f in sorted(base.glob("*/*.cook")) if want in (f.stem.casefold(), f"{f.parent.name}/{f.stem}".casefold())]


def missing(base):
    """{ingredient: [recipe file, ...]} for each one `config/aisle.conf` lacks, as `cook doctor aisle` reports them."""
    out = subprocess.run([COOK, "doctor", "aisle", "--show-recipes"], cwd=base, capture_output=True, text=True, check=True).stdout
    found = {}
    for line in out.splitlines():
        if m := re.fullmatch(r"  - (.+) \(\d+ recipes?\)", line):
            files = found.setdefault(m[1], [])
        elif line.startswith("      ") and found:
            files.append(line.strip())
    return found


def read_aisle():
    """config/aisle.conf as {section: [[name, alias, ...], ...]}, lines before any section under None."""
    path, sections, current = LIBRARY / "config" / AISLE, {None: []}, None
    for line in path.read_text().splitlines() if path.exists() else []:
        if m := re.fullmatch(r"\[(.+)\]", line.strip()):
            current = m[1]
            sections.setdefault(current, [])
        elif line.strip():
            sections[current].append([n.strip() for n in line.split("|")])
    return sections


def portuguese():
    """{name: the Portuguese singular} for each name config/aisle.conf knows: its line's first name."""
    return {n.lower(): g[0] for gs in read_aisle().values() for g in gs for n in g}


def aisle():
    """Draft config/aisle.conf with every ingredient it lacks, each in a section with its other names."""
    sections = read_aisle()
    groups = [g for gs in sections.values() for g in gs]
    known, firsts = {n.lower() for g in groups for n in g}, {g[0]: g for g in groups}
    lacking, joined, added = sorted(missing(LIBRARY)), [], 0
    for i in range(0, len(lacking), 20):
        batch = [n for n in lacking[i:i + 20] if n.lower() not in known]
        listing = "\n".join(f"[{s}] " + "; ".join("|".join(g) for g in gs) for s, gs in sections.items() if s)
        reply = ask_model(
            f"A shopping list's sections, each with its products (a product's names joined by |):\n{listing}\n\n"
            "For each ingredient below give its section; `names`, every name a shop sells it under: singular and "
            "plural, in Portuguese and in English; and `joins`, the first name of a product above only when it is the "
            "very same item under another name, else \"\". A kind, a form or a dish is another product: sal marinho "
            "is not sal, cayenne pepper is not pimenta, cooking oil is not azeite, pickled onions are not cebola, "
            "mashed potato is not batata.\nIngredients:\n" + "\n".join(f"- {n}" for n in batch),
            {"type": "object", "required": ["items"], "properties": {"items": {"type": "array", "items": {
                "type": "object", "required": ["name", "section", "joins", "names"], "properties": {
                    "name": {"type": "string"}, "section": {"enum": [s for s in sections if s]},
                    "joins": {"type": "string"}, "names": {"type": "array", "minItems": 2, "items": {"type": "string"}}}}}}})
        for x in reply["items"]:
            if x["name"] not in batch:
                continue
            names = []
            for n in (n.strip() for n in [x["name"], *x["names"]]):
                if n and n.lower() not in known:
                    names.append(n)
                    known.add(n.lower())
            if not names:
                continue
            if group := firsts.get(x["joins"]):
                group += names
                joined.append(f"{group[0]}: + {', '.join(names)}")
            else:
                sections[x["section"]].append(names)
                added += 1
    unplaced = [n for n in lacking if n.lower() not in known]
    DRAFTS.mkdir(exist_ok=True)
    blocks = [("" if s is None else f"[{s}]\n") + "\n".join("|".join(g) for g in gs) for s, gs in sections.items() if s or gs]
    (DRAFTS / AISLE).write_text("\n\n".join(blocks) + "\n")
    lines = [f"Drafted drafts/{AISLE}: {added} new products, {len(joined)} joined to existing ones."]
    if joined:
        lines += ["Joined, so check these: a wrong join merges two products in the shopping list."]
        lines += [f"- {j}" for j in joined]
    if unplaced:
        lines.append(f"Not placed: {', '.join(unplaced)}")
    print("\n".join(lines + ["", f"Review the file, then approve it to replace config/{AISLE}."]))


def lint(path):
    """What formatting.md asks of a recipe that code can check, as doubts: the user fixes each, so none can loop."""
    recipe = json.loads(subprocess.run([COOK, "recipe", "read", "--format", "json", str(path)],
                                       capture_output=True, text=True, check=True).stdout)
    names, twice = [x["name"] for x in recipe["ingredients"]], set()
    for section in recipe["sections"]:   # each `= Component =` may list its own ingredients
        used = [names[i["index"]] for c in section["content"] if c["type"] == "step"
                for i in c["value"]["items"] if i["type"] == "ingredient"]
        twice |= {n for n in used if used.count(n) > 1}
    meta = recipe["metadata"]["map"]
    found = [f"no {field}" for field in REQUIRED if field not in meta]
    found += [f"{field}: not a field formatting.md knows" for field in meta if field not in FIELDS]
    steps = [c["value"]["items"] for s in recipe["sections"] for c in s["content"] if c["type"] == "step"]
    if steps and not any(i["type"] == "ingredient" for i in steps[0]):
        found.append("the first paragraph is not the ingredient list: a description goes in the `description:` field")
    found += [f"{n}: listed twice, give one total" for n in sorted(twice)]
    for x in recipe["ingredients"]:
        quantity = (x["quantity"] or {}).get("value") or {}
        if quantity.get("type") == "text" and re.match(r"\d", quantity["value"]):
            found.append(f"{x['name']}: {{{quantity['value']}}} is text to cook and does not scale: one number, then % and the unit")
        if x["name"][:1].isupper():
            found.append(f"{x['name']}: lowercase, as the shop sells it")
        if part := PART.fullmatch(x["name"]):
            found.append(f"{x['name']}: name the product, {part.group(2)!r}, and put {part.group(1)!r} in the unit")
        unit = (x["quantity"] or {}).get("unit") or ""
        if unit.lower() in UNITS:
            found.append(f"{x['name']}: " + (f"write {unit!r} as {UNITS[unit.lower()]!r}" if UNITS[unit.lower()] else f"a plain count has no unit, not {unit!r}"))
    text = path.read_text()
    found += [f"{w!r} is Brazilian: write {pt!r}" for w, pt in BRAZILIAN.items() if re.search(rf"\b{w}s?\b", text, re.IGNORECASE)]
    return found


def add_checks(path, doubts):
    text = path.read_text()
    end = text.find("\n---\n", 3) + 5 if text.startswith("---\n") else 0
    path.write_text(text[:end] + "\n" + "".join(f"{CHECK} {d}\n" for d in doubts) + text[end:])


def promote_aisle():
    draft, conf = DRAFTS / AISLE, LIBRARY / "config" / AISLE
    if not draft.exists():
        sys.exit(f"No draft {draft}")
    with tempfile.TemporaryDirectory() as tmp:   # cook reads config/aisle.conf from the folder it runs in
        Path(tmp, "config").mkdir()
        shutil.copy(draft, Path(tmp, "config", AISLE))
        checked = subprocess.run([COOK, "doctor", "aisle"], cwd=tmp, capture_output=True, text=True)
    if checked.returncode != 0:
        sys.exit(f"The draft has issues, fix them first:\n{checked.stdout}{checked.stderr}")
    shutil.copyfile(conf, conf.with_name(f"{AISLE}.bak"))   # the share refuses chmod, which shutil.copy does
    shutil.move(draft, conf)
    print(f"Saved {AISLE} in {conf.parent}, the previous one as {AISLE}.bak. "
          f"{len(missing(LIBRARY))} ingredients are still missing from it.")


def ident(path):
    """A library recipe as plans link it: Pratos/Arroz de pato."""
    return f"{path.parent.name}/{path.stem}"


def relink(old, new):
    """Point the saved plans at a recipe that moved or was renamed, and say how many changed."""
    was, now, changed = ident(old), ident(new), 0
    for menu in (LIBRARY / "plans").glob("*.menu"):
        text = menu.read_text()
        if (relinked := text.replace(f"@./{was}{{", f"@./{now}{{").replace(f"sobras: {old.stem}", f"sobras: {new.stem}")) != text:
            menu.write_text(relinked)
            changed += 1
    relink_plan(was, now)
    return changed


def relink_plan(old, new):
    """Point the meals planner's current plan at a recipe that moved, or its next save writes the old one back."""
    if not PLAN.exists():
        return

    def renamed(node):
        if isinstance(node, list):
            return [renamed(x) for x in node]
        if not isinstance(node, dict):
            return node
        node = {k: renamed(v) for k, v in node.items()}
        if node.get("source") == "library" and node.get("id") == old:
            node |= {"id": new, "name": new.split("/")[-1], "url": node["url"].replace(f"/{quote(old)}.html", f"/{quote(new)}.html")}
        return node
    state = renamed(json.loads(PLAN.read_text()))
    state["exclude"] = [new if x == old else x for x in state.get("exclude", [])]
    tmp = PLAN.with_suffix(".tmp")
    tmp.write_text(json.dumps(state, ensure_ascii=False))
    tmp.replace(PLAN)


def promote(name, replace=None):
    if name == AISLE:
        return promote_aisle()
    if len(found := named(DRAFTS, name)) != 1:
        sys.exit(f"{len(found)} drafts are called {name}: name its folder too" if found else f"No draft {name} in a folder of {DRAFTS}")
    draft = found[0]
    photo, folder = draft.with_name(f"{draft.stem}.jpg"), draft.parent.name
    if folder not in CATEGORIES:
        sys.exit(f"{draft} is in no category: move it into one of {', '.join(f'drafts/{c}/' for c in CATEGORIES)}")
    old = None
    if replace:
        if len(found := named(LIBRARY, replace)) != 1:
            sys.exit(f"No single recipe {replace} in {LIBRARY} to replace")
        old = found[0]
    if (taken := named(LIBRARY, draft.stem)) and taken != [old]:
        sys.exit(f"{taken[0]} exists already: rename the draft first")
    if checks := [line for line in draft.read_text().splitlines() if line.startswith(CHECK)]:
        sys.exit(f"The draft has {len(checks)} open check(s): resolve each and delete its line first.\n" + "\n".join(checks))
    with tempfile.TemporaryDirectory() as tmp:   # validate scans a folder; this one holds only the draft
        shutil.copy(draft, tmp)
        checked = subprocess.run([COOK, "doctor", "validate", "--strict", "-b", tmp], capture_output=True, text=True)
    if checked.returncode != 0:
        sys.exit(f"The draft has issues, fix them first:\n{checked.stdout}{checked.stderr}")
    old_photo = old and old.with_name(f"{old.stem}.jpg")
    carried = old and old_photo.exists() and not photo.exists()   # the old photo moves in below
    if found := lint(draft):
        add_checks(draft, found)
        sys.exit(f"The draft needs {len(found)} fix(es), now `{CHECK}` lines in it:\n" + "\n".join(f"- {f}" for f in found))
    if old:
        shutil.move(old, DRAFTS / f"{old.stem}.cook.old")   # the user's undo
        if carried:
            shutil.move(old_photo, photo)
        elif old_photo.exists():   # the draft brings its own photo
            shutil.move(old_photo, DRAFTS / f"{old.stem}.jpg.old")
    saved = [f for f in (draft, photo) if f.exists()]
    (LIBRARY / folder).mkdir(exist_ok=True)
    for f in saved:
        shutil.move(f, LIBRARY / folder / f.name)
    where = LIBRARY / folder / draft.name
    lacking = [n for n, files in missing(LIBRARY).items() if f"{folder}/{draft.name}" in files]
    hint = f" Not in config/{AISLE}: {', '.join(lacking)}; `drafts aisle` drafts them." if lacking else ""
    if old:
        plans = relink(old, where) if ident(old) != ident(where) else 0
        hint = f" It replaces {ident(old)}, now drafts/{old.stem}.cook.old; {plans} plan(s) relinked.{hint}"
    print(f"Saved {' and '.join(f.name for f in saved)} in {LIBRARY / folder}.{hint}")


def main():
    p = argparse.ArgumentParser(description=__doc__)
    sub = p.add_subparsers(dest="command", required=True)
    draft = sub.add_parser("new", help="draft a recipe from a page, a video, or text")
    source = draft.add_mutually_exclusive_group(required=True)
    source.add_argument("url", nargs="?")
    source.add_argument("--text", action="store_true", help="read the recipe as plain text from stdin")
    draft.add_argument("--name", help="the name the user gave")
    draft.add_argument("--category", choices=list(CATEGORIES), help="the category the user gave")
    sub.add_parser("aisle", help=f"draft config/{AISLE} with the ingredients it lacks")
    approve = sub.add_parser("promote", help=f"move a reviewed draft, or {AISLE}, into the library")
    approve.add_argument("name")
    approve.add_argument("--replace", metavar="RECIPE", help="the library recipe the draft replaces")
    a = p.parse_args()
    if a.command == "promote":
        promote(a.name, a.replace)
    elif a.command == "aisle":
        aisle()
    else:
        new(a.url, sys.stdin.read() if a.text else None, a.name, a.category)


if __name__ == "__main__":
    main()
