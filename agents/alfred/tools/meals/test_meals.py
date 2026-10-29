import asyncio
import json
import re
import subprocess
import shutil
from datetime import date, timedelta

import pytest
from mcp.server.mcpserver.exceptions import UnexpectedToolError

import cookidoo as c
import meals as k
from test_cookidoo import LASANHA

WEEK = [date(2026, 10, 26) + timedelta(n) for n in range(7)]   # Monday to Sunday
WED = "2026-10-28"


def recipe(id, protein, difficulty="easy", source="library", servings=2):   # 2 servings: one day, no leftovers
    return {"source": source, "id": id, "cookidoo_id": id if source == "cookidoo" else None, "name": id,
            "url": f"https://recipes.test/{id}", "protein": protein,
            "minutes": 30, "difficulty": difficulty, "servings": servings}


MAINS = [recipe(f"meat{n}", "meat") for n in range(5)] + [recipe(f"fish{n}", "fish") for n in range(5)] + [recipe("veg", "vegetarian")]
SOUPS = [recipe(f"soup{n}", "vegetarian") for n in range(5)]


def plan(span=WEEK, mains=MAINS, planned=None, keep_mains=None, keep_soups=None, exclude=(), force=None, recent=frozenset()):
    return k.compose(span, SOUPS, mains, planned or {}, keep_mains or {}, keep_soups or {}, set(exclude), force, recent)


def mains_of(p):
    return [d["main"] for d in p["days"]]


def cooked(p):
    """The dishes in the order they are cooked, leftovers left out."""
    return [d["main"] for d in p["days"] if not d["leftover"]]


def kept(p, skip):
    return ({d["date"]: d["main"]["id"] for d in p["days"] if d["date"] != skip and not d["leftover"]},
            {b["start"]: b["soup"]["id"] for b in p["soups"]})


@pytest.mark.parametrize("ingredients, expected", [
    (["lombos de bacalhau", "batatas"], "fish"),          # a cut, not meat
    (["chicken breast", "mashed potato"], "meat"),        # "potato" is not the squid "pota"
    (["peito de frango", "miolo de camarão"], "both"),
    (["grão", "espinafres"], "vegetarian"),
])
def test_protein(ingredients, expected):
    assert k.protein(ingredients) == expected


def test_declared_protein_wins():
    assert k.protein(["frango"], "fish") == "fish"



def test_a_dish_lasts_as_many_days_as_its_servings_feed_the_household():
    mains = [recipe(f"meat{n}", "meat", servings=4) for n in range(3)] + [recipe(f"fish{n}", "fish", servings=4) for n in range(3)]
    p = plan(mains=mains)
    assert [d["leftover"] for d in p["days"]] == [False, True, False, True, False, True, False]
    assert all(d["main"] == p["days"][i - 1]["main"] for i, d in enumerate(p["days"]) if d["leftover"])
    assert not any(k.clash(a, b) for a, b in zip(cooked(p), cooked(p)[1:])) and p["notes"] == []


def test_a_my_week_dish_brings_its_leftovers():
    p = plan(planned={WED: recipe("lasanha", "fish", source="cookidoo", servings=6)})
    assert [d["main"]["id"] for d in p["days"][2:5]] == ["lasanha"] * 3 and [d["leftover"] for d in p["days"][2:5]] == [False, True, True]
    assert not any(k.clash(a, b) for a, b in zip(cooked(p), cooked(p)[1:]))


def test_swapping_a_day_of_leftovers_changes_the_dish_they_come_from():
    mains = [recipe(f"meat{n}", "meat", servings=4) for n in range(4)] + [recipe(f"fish{n}", "fish", servings=4) for n in range(4)]
    state = {"plan": plan(mains=mains)}
    assert k.cooked_on(state, "2026-10-27") == "2026-10-26" and k.cooked_on(state, "2026-10-26") == "2026-10-26"


def test_week_follows_the_rules():
    p = plan()
    mains = mains_of(p)
    assert len({m["id"] for m in mains}) == 7
    assert not any(k.clash(a, b) for a, b in zip(mains, mains[1:]))
    assert [(b["start"], b["end"]) for b in p["soups"]] == [
        ("2026-10-26", "2026-10-28"), ("2026-10-29", "2026-10-31"), ("2026-11-01", "2026-11-01")]
    assert p["notes"] == []


def test_no_two_hard_dishes_in_a_row():
    mains = [recipe("meat-hard", "meat", "hard"), recipe("fish-hard", "fish", "hard"), recipe("meat", "meat"), recipe("fish", "fish")]
    p = plan(span=WEEK[:4], mains=mains)
    assert not any(a["difficulty"] == b["difficulty"] == "hard" for a, b in zip(mains_of(p), mains_of(p)[1:]))


def test_my_week_is_kept_and_respected():
    cookidoo = recipe("lasanha", "fish", source="cookidoo")
    p = plan(planned={WED: cookidoo})
    days = {d["date"]: d for d in p["days"]}
    assert days[WED]["main"] == cookidoo and days[WED]["fixed"] == "cookidoo"
    assert not any(k.clash(a, b) for a, b in zip(mains_of(p), mains_of(p)[1:]))


def test_swap_changes_only_that_day():
    p = plan()
    mains, soups = kept(p, WED)
    q = plan(keep_mains=mains, keep_soups=soups, exclude=[next(d["main"]["id"] for d in p["days"] if d["date"] == WED)])
    changed = [a["date"] for a, b in zip(p["days"], q["days"]) if a["main"]["id"] != b["main"]["id"]]
    assert changed == [WED]
    assert p["soups"] == q["soups"]


def test_forced_protein_wins_with_a_note():
    p = plan()
    mains, soups = kept(p, WED)
    neighbour = next(d["main"]["protein"] for d in p["days"] if d["date"] == "2026-10-27")
    q = plan(keep_mains=mains, keep_soups=soups, force={WED: neighbour})
    assert next(d["main"]["protein"] for d in q["days"] if d["date"] == WED) == neighbour
    assert q["notes"]


def test_different_shares_nothing():
    p = plan()
    q = plan(exclude=[m["id"] for m in mains_of(p)] + [b["soup"]["id"] for b in p["soups"]])
    assert not {m["id"] for m in mains_of(p) if m} & {m["id"] for m in mains_of(q) if m}


@pytest.mark.parametrize("text, iso", [("quarta", WED), ("qua", WED), ("28/10", WED), (WED, WED), ("sabado", "2026-10-31"), ("4f", WED), ("4ª feira", WED)])
def test_resolve(text, iso):
    assert k.resolve({"plan": plan()}, text) == iso


def test_resolve_outside_the_plan():
    with pytest.raises(k.ToolError):
        k.resolve({"plan": plan()}, "15/11")


def test_a_draft_asks_to_be_saved_and_a_committed_plan_links_its_page():
    draft = k.summary({"start": "2026-10-26", "plan": plan()})
    lines = draft.splitlines()
    assert lines[0].startswith("seg 26/10: ") and " · sopa: " in lines[0] and " · sopa: " not in lines[1]
    assert lines[-1] == "Rascunho, ainda não guardado. Guardo este plano?" and "https://" not in draft
    committed = k.summary({"start": "2026-10-26", "plan": plan(), "committed": True})
    assert committed.splitlines()[-1] == "Plano guardado: https://recipes.test/menu/plans/2026-10-26.html"


def test_the_summary_names_the_leftovers():
    meat = recipe("meat", "meat", servings=4)
    lines = k.summary({"start": "2026-10-26", "plan": plan(span=WEEK[:2], mains=[meat])}).splitlines()
    assert lines[0].startswith("seg 26/10: meat · sopa: soup") and lines[1] == "ter 27/10: sobras de meat"


def test_recent_dishes_come_last_among_those_that_fit():
    first = plan(span=WEEK[:4])   # uses 4 of the 11 mains, leaving enough of each protein
    again = plan(span=WEEK[:4], recent={m["id"] for m in mains_of(first)})
    assert not {m["id"] for m in mains_of(first)} & {m["id"] for m in mains_of(again)}
    assert not any(k.clash(a, b) for a, b in zip(mains_of(again), mains_of(again)[1:]))


def test_a_my_week_dish_outside_the_library_is_plain_text_in_the_menu():
    text = k.menu(plan(planned={WED: recipe("lasanha", "fish", source="cookidoo")}))
    assert "- lasanha (Cookidoo)" in text and "@./lasanha" not in text
    assert text.count("\n== ") == 7 and "Jantar: \\\n- @./" in text


def test_never_is_not_planned_and_liked_comes_first():
    mains = [recipe(f"meat{n}", "meat") for n in range(3)] + [recipe(f"fish{n}", "fish") for n in range(3)]
    mains = [{**r, "rating": "never" if r["id"] == "meat0" else "liked" if r["id"] == "fish2" else None} for r in mains]
    p = plan(span=WEEK[:4], mains=mains)
    assert "meat0" not in {m["id"] for m in mains_of(p)} and "fish2" in {m["id"] for m in mains_of(p)}


def test_library_reads_the_fixtures():
    found = k.library()
    assert [r["name"] for r in found["soup"]] == ["Sopa de legumes"]
    assert {r["name"]: r["protein"] for r in found["main"]} == {"Bacalhau com natas": "fish", "Frango assado": "meat"}
    assert all(" " not in r["url"] for r in found["main"])


def test_groceries_sum_and_include_the_soups():
    found = k.library()
    p = k.compose(WEEK[:4], found["soup"], found["main"], {}, {}, {}, set())   # both mains, two days each
    cookidoo_recipes, items = k.groceries({"plan": p})
    assert cookidoo_recipes == []
    assert ("alho", "4 dentes") in items and ("cenoura", "2 unidades") in items   # leftovers are not shopped for twice


def test_propose_week_shows_the_current_plan_instead_of_replanning():
    p = plan()
    k.save({"start": "2026-10-26", "days": 7, "exclude": [], "plan": p})
    assert asyncio.run(k.propose_week("2026-10-26", 7)) == k.summary(k.load())   # no Cookidoo needed, so nothing was regenerated


@pytest.fixture
def library(tmp_path, monkeypatch):
    for f in k.LIBRARY.glob("*.cook"):
        shutil.copy(f, tmp_path)
    monkeypatch.setattr(k, "LIBRARY", tmp_path)
    monkeypatch.setattr(c, "LIBRARY", tmp_path)
    return tmp_path


def test_a_synced_copy_is_planned_and_sent_by_its_cookidoo_id(library):
    c.apply([LASANHA], {})
    found = k.library()
    copy = next(r for r in found["main"] if r["name"] == "Lasanha de atum")
    assert copy["source"] == "cookidoo" and copy["cookidoo_id"] == "r20307" and copy["protein"] == "fish"
    p = k.compose(WEEK[:3], found["soup"], [copy], {}, {}, {}, set())   # 6 servings: it lasts all three days
    cookidoo_recipes, items = k.groceries({"plan": p})
    assert k.my_week_dishes({"plan": p}) == [["2026-10-26", "r20307"]] and [r["cookidoo_id"] for r in cookidoo_recipes] == ["r20307"]
    assert not any(name == "atum em lata" for name, _ in items)   # Cookidoo's own list covers it


def test_groceries_follow_the_library_s_cook_config(library):
    (library / "Puré.cook").write_text("---\nservings: 2\ntags: [mains]\n---\n\n@batata{2}, @sal{}.\n\nCozer.\n")
    (library / "config").mkdir()
    (library / "config" / "aisle.conf").write_text("[legumes]\nbatata|batatas\n")
    (library / "config" / "pantry.conf").write_text("[despensa]\nalho = {}\nsal = {}\n")
    found = k.library()
    p = k.compose(WEEK[:3], found["soup"], [r for r in found["main"] if r["name"] in ("Frango assado", "Puré")], {}, {}, {}, set())
    _, items = k.groceries({"plan": p})
    assert ("batata", "2, 500 g") in items and not any(name in ("alho", "sal", "batatas") for name, _ in items)


def test_rating_a_dish_tags_it_and_keeps_its_other_tags(library):
    path = library / "Frango assado.cook"
    text = path.read_text()
    path.unlink()
    path.write_text(text)
    assert asyncio.run(k.rate_dish("frango", "never")) == "Anotado: Frango assado não volta a ser planeado."
    assert k.read(path)["metadata"]["map"]["tags"] == ["mains", "never"]
    asyncio.run(k.rate_dish("frango", None))
    assert k.read(path)["metadata"]["map"]["tags"] == ["mains"]


def test_a_frontmatter_that_cannot_be_rewritten_is_left_alone(library):
    path = library / "Frango assado.cook"
    path.unlink()
    path.write_text("---\nservings: 4\ntags:\n  - mains\n---\n\n@frango{1%kg}.\n\nAssar.\n")
    before = path.read_text()
    with pytest.raises(k.ToolError):
        asyncio.run(k.rate_dish("frango", "liked"))
    assert path.read_text() == before


def test_an_unreadable_recipe_is_left_out_with_a_note(library):
    (library / "Frango assado.cook").chmod(0)
    found = k.library()
    assert found["broken"] == ["Frango assado"] and [r["name"] for r in found["main"]] == ["Bacalhau com natas"]


@pytest.fixture
def fake_cookidoo(tmp_path, monkeypatch):
    """A stand-in for the cookidoo program: answers `week`, records what `plan` and `send` received, or fails on demand."""
    script, sent = tmp_path / "cookidoo", tmp_path / "sent.json"
    script.write_text(f"""#!/bin/sh
[ -e {tmp_path}/fail ] && {{ echo "cookidoo week: Not signed in" >&2; exit 1; }}
[ -e {tmp_path}/slow ] && sleep 5
case "$1" in
  week) echo '{{"2026-10-26": [{{"id": "r20307", "name": "Lasanha de atum", "url": "https://cookidoo.test/r20307"}}]}}' ;;
  plan) cat >> {tmp_path}/planned.jsonl; echo >> {tmp_path}/planned.jsonl; echo '{{}}' ;;
  send) cat > {sent}; echo '{{"recipes": ["r20307"], "items": ["i1"]}}' ;;
esac
""")
    script.chmod(0o755)
    monkeypatch.setattr(k, "COOKIDOO", str(script))
    monkeypatch.setattr(k, "PLAN", tmp_path / "plan.json")
    return tmp_path, sent


def test_my_week_maps_onto_the_synced_copy(library, fake_cookidoo):
    c.apply([LASANHA], {})
    soups, mains, planned, notes, recently = asyncio.run(k.gather(WEEK[:3]))
    assert planned["2026-10-26"]["name"] == "Lasanha de atum" and planned["2026-10-26"]["protein"] == "fish" and notes == []


def test_what_a_commit_put_in_my_week_is_not_a_fixed_day(library, fake_cookidoo):
    c.apply([LASANHA], {})
    k.save({"start": "2026-10-26", "days": 3, "in_week": [["2026-10-26", "r20307"]], "plan": {}})
    assert asyncio.run(k.gather(WEEK[:3]))[2] == {}


def test_cookidoo_down_still_plans_with_a_note(library, fake_cookidoo):
    (fake_cookidoo[0] / "fail").touch()
    soups, mains, planned, notes, recently = asyncio.run(k.gather(WEEK[:3]))
    assert planned == {} and mains and notes == ["Sem acesso ao Cookidoo (cookidoo week: Not signed in); o plano não tem em conta o My week."]


def plan_orders(fake):
    path = fake[0] / "planned.jsonl"
    return [json.loads(line) for line in path.read_text().splitlines() if line.strip()] if path.exists() else []


def test_commit_sends_the_groceries_once(library, fake_cookidoo):
    c.apply([LASANHA], {})
    found = k.library()
    k.save({"start": "2026-10-27", "days": 5, "exclude": [],
            "plan": k.compose(WEEK[1:6], found["soup"], found["main"], {}, {}, {}, set())})
    assert "Na lista de compras do Cookidoo: ingredientes de 1 receita(s)" in asyncio.run(k.commit_plan())
    cooked_on = next(d["date"] for d in k.load()["plan"]["days"] if d["main"]["cookidoo_id"] and not d["leftover"])
    assert plan_orders(fake_cookidoo) == [{"add": [[cooked_on, "r20307"]], "remove": []}]   # and so fills My week
    order = json.loads(fake_cookidoo[1].read_text())
    assert order["recipes"] == ["r20307"] and order["items"] and not any("atum" in i for i in order["items"]) and order["sent"] is None
    asyncio.run(k.commit_plan())
    assert json.loads(fake_cookidoo[1].read_text()) == order   # nothing changed, so nothing was sent again
    frango = library / "Frango assado.cook"
    text = frango.read_text()
    frango.unlink()   # the copy is read-only, like the fixture it came from
    frango.write_text(text.replace("{1%kg}", "{2%kg}"))
    asyncio.run(k.commit_plan())   # a recipe changed after sending: the earlier send is replaced
    resent = json.loads(fake_cookidoo[1].read_text())
    assert resent["sent"] == {"recipes": ["r20307"], "items": ["i1"], "texts": order["items"]} and resent["items"] != order["items"]


def test_commit_puts_cookidoo_dishes_in_my_week_and_a_later_commit_takes_out_only_its_own(library, fake_cookidoo):
    c.apply([LASANHA], {})
    found = k.library()
    lasanha = next(r for r in found["main"] if r["cookidoo_id"])
    k.save({"start": "2026-10-27", "days": 3, "exclude": [], "in_week": [["2026-10-01", "r1"], ["2999-01-01", "r2"]],
            "plan": k.compose(WEEK[1:4], found["soup"], [lasanha], {}, {}, {}, set())})
    assert "No My week: 1 prato(s) do Cookidoo." in asyncio.run(k.commit_plan())
    assert k.load()["in_week"] == [["2999-01-01", "r2"], ["2026-10-27", "r20307"]]   # a past day is forgotten, another week kept
    k.save({**k.load(), "plan": k.compose(WEEK[1:4], found["soup"], [r for r in found["main"] if not r["cookidoo_id"]], {}, {}, {}, set())})
    asyncio.run(k.commit_plan())
    assert plan_orders(fake_cookidoo)[-1] == {"add": [], "remove": [["2026-10-27", "r20307"]]}
    assert k.load()["in_week"] == [["2999-01-01", "r2"]]


def test_commit_without_cookidoo_still_saves_and_says_so(library, fake_cookidoo):
    c.apply([LASANHA], {})
    found = k.library()
    k.save({"start": "2026-10-27", "days": 3, "exclude": [], "plan": k.compose(WEEK[1:4], found["soup"], found["main"], {}, {}, {}, set())})
    (fake_cookidoo[0] / "fail").touch()
    said = asyncio.run(k.commit_plan())
    assert "O My week não foi atualizado" in said and "A lista de compras não foi atualizada" in said
    assert k.load()["committed"] and "in_week" not in k.load() and not k.load().get("shopping")


def test_the_menu_is_cooklang_that_cook_can_shop_from_and_is_written_once(library):
    found = k.library()
    state = {"start": "2026-10-26", "days": 4, "plan": k.compose(WEEK[:4], found["soup"], found["main"], {}, {}, {}, set())}
    k.write_menu(state)
    path = library / "plans" / "2026-10-26.menu"
    out = subprocess.run([k.COOK, "shopping-list", "--plain", "plans/2026-10-26.menu"], cwd=library,
                         capture_output=True, text=True, check=True).stdout
    assert re.search(r"alho\s+4 dentes", out)   # both mains and the soup, followed through the references, leftovers once
    assert "- sobras: " in path.read_text()
    before = path.stat().st_mtime_ns
    k.write_menu(state)
    assert path.stat().st_mtime_ns == before


def test_commit_writes_the_menu_and_only_committed_plans_of_the_last_two_weeks_count(library, fake_cookidoo):
    found = k.library()
    k.save({"start": "2026-10-26", "days": 2, "exclude": [], "committed": False,
            "plan": k.compose(WEEK[:2], found["soup"], found["main"], {}, {}, {}, set())})
    assert not (library / "plans").exists()   # a draft writes nothing
    assert "Plano guardado: https://recipes.test/menu/plans/2026-10-26.html" in asyncio.run(k.commit_plan())
    assert (library / "plans" / "2026-10-26.menu").exists() and k.load()["committed"]
    (library / "plans" / "2026-10-05.menu").write_text("Jantar: \\\n- @./Old dish{}\n")   # three weeks before
    assert k.recent(date(2026, 11, 2)) == {m["id"] for m in [*cooked(k.load()["plan"]), k.load()["plan"]["soups"][0]["soup"]]}
    assert k.recent(date(2026, 10, 26)) == set()   # its own week does not count


def test_a_hung_cookidoo_ends_in_a_clear_error_not_a_hung_chat(library, fake_cookidoo, monkeypatch):
    (fake_cookidoo[0] / "slow").touch()
    monkeypatch.setattr(k, "COOKIDOO_TIMEOUT", 1)
    with pytest.raises(k.ToolError, match="sem resposta do Cookidoo em 1 s"):
        asyncio.run(k.cookidoo("week", "2026-10-26", "7"))


def test_swap_day_to_an_untagged_recipe_says_how_to_plan_it(library, fake_cookidoo):
    found = k.library()
    k.save({"start": "2026-10-26", "days": 3, "exclude": [], "plan": k.compose(WEEK[:3], found["soup"], found["main"], {}, {}, {}, set())})
    (library / "Folhado de pato.cook").write_text("---\nservings: 6\ntime: 30\ntags: [simples]\n---\n\n@folhado de pato{2}.\n")
    with pytest.raises(k.ToolError, match="`mains` tag"):
        asyncio.run(k.swap_day("4f", recipe="Folhado de pato"))


SLOW = [{**recipe(f"slow{p}{n}", p, difficulty="medium"), "minutes": 90} for p in ("meat", "fish") for n in range(3)]


def test_a_limit_keeps_every_main_quick_and_easy():
    limits = {d.isoformat(): {"max_minutes": 30, "difficulty": "easy"} for d in WEEK}
    p = k.compose(WEEK, SOUPS, MAINS + SLOW, {}, {}, {}, set(), limits=limits)
    assert all(m["minutes"] <= 30 and m["difficulty"] == "easy" for m in mains_of(p))


def test_a_day_limit_that_nothing_fits_says_so():
    p = k.compose(WEEK[:2], SOUPS, SLOW, {}, {}, {}, set(), limits={WEEK[0].isoformat(): {"max_minutes": 20}})
    assert mains_of(p)[0] is None and p["notes"] == ["seg 26/10: não há mais pratos (até 20 min) por usar."]


def test_within_needs_the_field_it_limits():
    assert not k.within({"minutes": None, "difficulty": "easy"}, {"max_minutes": 30})
    assert k.within({"minutes": 40, "difficulty": "medium"}, {"difficulty": "medium"})
    with pytest.raises(k.ToolError):
        k.check(30, "quick")


def test_a_week_limit_is_kept_by_a_different_plan_and_shown(library, fake_cookidoo):
    said = asyncio.run(k.propose_week("2026-10-26", 3, max_minutes=60))
    assert k.load()["limit"] == {"max_minutes": 60} and "Pratos até 60 min." in said
    asyncio.run(k.propose_week("2026-10-26", 3, different=True))
    assert k.load()["limit"] == {"max_minutes": 60}


def test_a_plan_not_shown_lately_is_not_saved(library, fake_cookidoo):
    found = k.library()
    k.save({"start": "2026-10-27", "days": 5, "exclude": [],
            "plan": k.compose(WEEK[1:6], found["soup"], found["main"], {}, {}, {}, set())})
    k.PLAN.write_text(json.dumps(k.load() | {"shown": "2026-10-20T09:00:00"}))
    with pytest.raises(k.ToolError, match="not shown in the last hour"):
        asyncio.run(k.commit_plan())
    asyncio.run(k.propose_week("2026-10-27", 5))
    assert "Plano guardado" in asyncio.run(k.commit_plan())


def test_a_tool_error_reaches_the_model_with_its_reason(library, fake_cookidoo):
    with pytest.raises(k.ToolError, match="There is no plan yet") as raised:
        asyncio.run(k.mcp.call_tool("swap_day", {"day": "qua"}))
    assert not isinstance(raised.value, UnexpectedToolError)   # mcp hides the text of any other exception


def test_showing_the_plan_mid_week_keeps_the_one_being_cooked(library, fake_cookidoo, monkeypatch):
    asyncio.run(k.propose_week("2026-10-26", 7))
    before = k.load()

    class Wednesday(date):
        @classmethod
        def today(cls):
            return date(2026, 10, 28)
    monkeypatch.setattr(k, "date", Wednesday)
    asyncio.run(k.propose_week())
    assert k.load() == before


def test_find_prefers_the_exact_name():
    dishes = [{"name": "Arroz de Pato no forno"}, {"name": "Arroz de Pato"}]
    assert k.find(dishes, "arroz de pato")["name"] == "Arroz de Pato"
