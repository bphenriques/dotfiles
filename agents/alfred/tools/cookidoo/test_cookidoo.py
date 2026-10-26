import asyncio
import shutil

import pytest
from cookidoo_api.types import (CookidooAdditionalItem, CookidooCalendarDay, CookidooCalendarDayRecipe, CookidooCategory,
                                CookidooIngredient, CookidooRecipeStep, CookidooRecipeStepGroup, CookidooShoppingRecipeDetails)

import cookidoo as c

# Recorded from Cookidoo (r20307), trimmed, as the adapter returns it: the shape the copy works on.
LASANHA = {
    "id": "r20307", "name": "Lasanha de atum", "url": "https://cookidoo.pt/recipes/recipe/pt-PT/r20307",
    "image": "https://assets.test/lasanha", "servings": 6, "minutes": 60, "difficulty": "easy",
    "categories": ["Pratos de peixe e marisco"], "notes": [], "utensils": ["forno", "pirex"],
    "ingredients": [{"name": "queijo da Ilha", "amount": "100 g"}, {"name": "dentes de alho", "amount": "3"},
                    {"name": "fermento p/ bolos", "amount": "1.5 c. chá de"}, {"name": "sal", "amount": "q.b."},
                    {"name": "atum em lata", "amount": "360 g"}],
    "steps": [{"title": "", "texts": [
        "Coloque no copo o queijo e rale <nobr>5 seg/vel 9</nobr>. Retire e reserve.",
        "Adicione o atum e envolva <nobr>5 seg/vel 3</nobr> &amp; sirva @ 180°C.",
    ]}],
}


@pytest.fixture
def library(tmp_path, monkeypatch):
    for f in c.LIBRARY.glob("*.cook"):
        shutil.copy(f, tmp_path)
    monkeypatch.setattr(c, "LIBRARY", tmp_path)
    return tmp_path


def test_the_adapter_reads_cookidoo_api_s_real_types():
    """The contract with cookidoo-api: a renamed field in a new version fails this, not the daily sync."""
    details = CookidooShoppingRecipeDetails(
        id="r20307", name="Lasanha de atum", url="https://cookidoo.pt/recipes/recipe/pt-PT/r20307", thumbnail=None,
        image="https://assets.test/lasanha", difficulty="easy", notes=[], collections=[], nutrition_groups=[],
        categories=[CookidooCategory(id="c1", name="Pratos de peixe e marisco", notes="")], utensils=["forno"],
        serving_size=6, active_time=2100, total_time=3600,
        ingredients=[CookidooIngredient(id="i1", name="queijo da Ilha", description="100 g")],
        step_groups=[CookidooRecipeStepGroup(title="Molho", recipe_steps=[CookidooRecipeStep(title="", formatted_text="Rale.")])],
    )
    assert c.recipe_of(details) == {
        "id": "r20307", "name": "Lasanha de atum", "url": "https://cookidoo.pt/recipes/recipe/pt-PT/r20307",
        "image": "https://assets.test/lasanha", "servings": 6, "minutes": 60, "difficulty": "easy",
        "categories": ["Pratos de peixe e marisco"], "ingredients": [{"name": "queijo da Ilha", "amount": "100 g"}],
        "steps": [{"title": "Molho", "texts": ["Rale."]}], "notes": [], "utensils": ["forno"],
    }
    day = CookidooCalendarDay(id="2026-10-26", title="", recipes=[CookidooCalendarDayRecipe(
        id="r20307", name="Lasanha de atum", total_time=3600, thumbnail=None, image=None, url="https://cookidoo.test/r20307")])
    assert c.day_of(day) == ("2026-10-26", [{"id": "r20307", "name": "Lasanha de atum", "url": "https://cookidoo.test/r20307"}])
    assert c.item_of(CookidooAdditionalItem(id="i1", name="cebola 3", is_owned=True)) == ("i1", "cebola 3", True)


def test_desserts():
    assert c.DESSERT_RE.search("Bolo de cenoura com cobertura de chocolate")
    assert not c.DESSERT_RE.search("Caril de batata-doce")


def test_cookidoo_recipe_becomes_valid_cooklang():
    text = c.to_cook(LASANHA, "Lasanha de atum", ["mains", "cookidoo"], True)
    parsed = c.read(text=text)
    meta = parsed["metadata"]["map"]
    assert meta["tags"] == ["mains", "cookidoo"] and meta["course"] == "main" and meta["servings"] == 6 and meta["time"] == 60
    assert meta["cookidoo"] == "r20307"
    assert [i["name"] for i in parsed["ingredients"]] == ["queijo da Ilha", "dentes de alho", "fermento p/ bolos", "sal", "atum em lata"]
    assert "@fermento p/ bolos{1.5%c. chá}" in text and "@sal{}(q.b.)" in text
    assert "<nobr>" not in text and "& sirva a 180°C" in text and "> Utensílios: forno, pirex" in text


def test_sync_creates_then_changes_nothing(library):
    assert c.apply([LASANHA], {"r20307": b"jpeg"}) == ["Lasanha de atum: nova"]
    before = {p.name: p.stat().st_mtime_ns for p in library.iterdir()}
    assert c.apply([LASANHA], {"r20307": b"jpeg"}) == ["Lasanha de atum: sem alterações"]
    assert {p.name: p.stat().st_mtime_ns for p in library.iterdir()} == before
    assert (library / "Lasanha de atum.jpg").read_bytes() == b"jpeg"


def test_sync_follows_cookidoo_but_keeps_your_tags(library):
    c.apply([LASANHA], {})
    path = library / "Lasanha de atum.cook"
    path.write_text(path.read_text().replace('["mains", "cookidoo"]', '["baby-food", "cookidoo"]'))
    assert c.apply([{**LASANHA, "servings": 4}], {}) == ["Lasanha de atum: atualizada"]
    meta = c.read(path)["metadata"]["map"]
    assert meta["servings"] == 4 and meta["tags"] == ["baby-food", "cookidoo"] and "course" not in meta


def test_a_copy_from_before_the_cookidoo_field_is_recognised_and_gets_it(library):
    path = library / "Lasanha de atum.cook"
    path.write_text(c.to_cook(LASANHA, "Lasanha de atum", ["mains", "cookidoo"], False).replace('cookidoo: "r20307"\n', ""))
    assert c.apply([LASANHA], {}) == ["Lasanha de atum: atualizada"]
    assert c.read(path)["metadata"]["map"]["cookidoo"] == "r20307"


def test_sync_never_touches_a_file_of_yours(library):
    mine = library / "Lasanha de atum.cook"
    mine.write_text("---\nservings: 2\ntags: [mains]\nsource: https://cookidoo.pt/recipes/recipe/pt-PT/r20307\n---\n\n@atum{1%lata}.\n\nMisturar.\n")
    before = mine.read_text()
    assert c.apply([LASANHA], {}) == ["Lasanha de atum: já existe uma receita tua com este nome, não foi tocada"]
    assert mine.read_text() == before   # a Cookidoo link alone, without the cookidoo tag, does not make it a copy


def test_clean_leaves_only_prose():
    nasty = "= Título > @sal e #panela ~{5%min} -- comentário [- bloco -] {x} <b>negrito</b> &amp; fim"
    parsed = c.read(text=c.to_cook({**LASANHA, "steps": [{"title": "", "texts": [nasty]}]}, "Lasanha de atum", ["mains"], False))
    assert {i["name"] for i in parsed["ingredients"]} == {c.ingredient_name(i) for i in LASANHA["ingredients"]}
    assert parsed["cookware"] == [] and parsed["timers"] == []
    assert "Título" in c.clean(nasty) and "negrito & fim" in c.clean(nasty)


def test_a_dotted_name_gets_its_own_photo(library):
    c.apply([{**LASANHA, "id": "r1", "name": "Bolo 1.5 kg"}], {"r1": b"jpeg"})
    assert (library / "Bolo 1.5 kg.jpg").read_bytes() == b"jpeg" and not (library / "Bolo 1.jpg").exists()


def test_a_photo_of_yours_is_never_replaced(library):
    (library / "Lasanha de atum.jpg").write_bytes(b"mine")
    c.apply([LASANHA], {"r20307": b"theirs"})
    assert (library / "Lasanha de atum.jpg").read_bytes() == b"mine"
    assert "image" not in c.read(library / "Lasanha de atum.cook")["metadata"]["map"]


def test_an_unreadable_file_is_not_taken_for_a_copy(library):
    (library / "Frango assado.cook").chmod(0)
    assert c.apply([LASANHA], {}) == ["Lasanha de atum: nova"]


def test_only_sane_images_are_kept():
    assert c.photo("image/jpeg", b"x") == b"x"
    assert c.photo("text/html", b"x") is None
    assert c.photo("image/jpeg", b"x" * (c.MAX_IMAGE + 1)) is None


def test_a_download_is_read_whole_and_capped():
    async def chunks(*parts):
        for part in parts:
            yield part
    assert asyncio.run(c.body(chunks(b"ab", b"cd", b"ef"))) == b"abcdef"
    big = asyncio.run(c.body(chunks(*[b"x" * 2**20] * 20)))
    assert c.MAX_IMAGE < len(big) <= c.MAX_IMAGE + 2**20 and c.photo("image/jpeg", big) is None


def test_a_resend_replaces_only_what_the_last_one_added_and_keeps_what_is_ticked():
    have = {"i1": ("cebola 3", False), "i2": ("alho 4 dentes", True), "mine": ("pão", False)}   # i3 removed by hand
    order = {"recipes": ["r2", "r3"], "items": ["cebola 2", "alho 4 dentes"], "sent": {"recipes": ["r1", "r2"], "items": ["i1", "i2", "i3"]}}
    assert c.restock(order, have) == (["r1"], ["i1"], ["r3"], ["cebola 2"], ["i2"])
    assert c.restock({"recipes": ["r1"], "items": ["sal"], "sent": None}, have) == ([], [], ["r1"], ["sal"], [])


def test_my_week_is_matched_by_day_and_id():
    have = {"2026-10-26": [{"id": "r20307", "name": "Lasanha de atum", "url": "u"}]}
    assert c.held(["2026-10-26", "r20307"], have)
    assert not c.held(["2026-10-27", "r20307"], have) and not c.held(["2026-10-26", "r1"], have)
