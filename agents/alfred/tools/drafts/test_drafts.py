import email.message
import io
import json
import subprocess
from pathlib import Path

import pytest

import drafts as d

NAMES = {"cebola": "cebola", "cebolas": "cebola", "onion": "cebola", "onions": "cebola", "alho": "alho", "garlic": "alho"}


def recipe(*ingredients, evidence=None, **extra):
    return {"servings": 4, "time": 30, "prep": None, "cook": None, "category": "Pratos", "difficulty": "easy",
            "url": "https://example.test/r", "author": None, "image": "https://example.test/r.jpg", "steps": ["Mix."], "evidence": evidence,
            "ingredients": [dict(zip(("name", "quantity", "unit", "note", "line"), x)) for x in ingredients]} | extra


@pytest.mark.parametrize("text,expected", [
    ("100g pancetta", {100}), ("½ limão", {0.5}), ("1,5 l água", {1.5}), ("duas cebolas", {2}), ("1/2 tsp", {0.5}), ("3.8/5", {3.8, 5}),
])
def test_numbers(text, expected):
    assert expected <= d.numbers(text)


def test_check_empties_a_quantity_the_source_does_not_state():
    r = recipe(("alho", "2", "dentes", "", "alho picado"))
    doubts = d.check(r, NAMES)
    assert r["ingredients"][0]["quantity"] == ""
    assert doubts == ["alho: the source does not say 2 dentes, left empty"]


def test_check_keeps_stated_quantities_and_names_them_in_portuguese():
    r = recipe(("cebolas", "2", "", "", "2 cebolas"), ("alho", "6", "dente", "", "6 dente alho"),
               ("garlic", "2", "cloves", "", "2 garlic cloves"), ("butter", "1", "tbsp", "", "1 tbsp butter"))
    assert d.check(r, NAMES) == []
    assert [(x["name"], x["quantity"], x["unit"]) for x in r["ingredients"]] == [
        ("cebola", "2", ""), ("alho", "6", "dentes"), ("alho", "2", "dentes"), ("butter", "1", "c. sopa")]


def test_translate_flags_what_the_model_changed(monkeypatch):
    r = recipe(("alho", "1", "", "peeled", None), name="Garlic toast", steps=["Toast 2 slices.", "Rub with garlic."], notes=[])
    monkeypatch.setattr(d, "ask_model", lambda prompt, schema: {
        "name": "Torrada de alho", "steps": ["1. Torrar 3 fatias.", "2. Esfregar com alho."], "notes": ["descascado"]})
    assert d.translate(r)
    assert (r["name"], r["steps"][1], r["ingredients"][0]["note"]) == ("Torrada de alho", "Esfregar com alho.", "descascado")
    assert r["notes"] == ["the name, steps and notes were translated by the model", "step 1: the translation changed its numbers"]
    monkeypatch.setattr(d, "ask_model", lambda prompt, schema: {"name": r["name"], "steps": r["steps"], "notes": ["descascado"]})
    assert not d.translate(r)


def test_check_flags_an_unused_number_but_not_salt_to_taste():
    r = recipe(("sal", "", "", "", "sal q.b."), ("pancetta", "", "", "", "100g pancetta"))
    assert d.check(r, NAMES) == ["pancetta: no quantity"]


class Page(io.BytesIO):
    headers = email.message.Message()


def test_a_page_whose_recipe_data_lacks_what_its_steps_use_is_read_from_its_text(monkeypatch):
    data = {"@context": "https://schema.org", "@type": "Recipe", "name": "Cookies", "recipeIngredient": ["120g de farinha"],
            "recipeInstructions": [{"@type": "HowToStep", "text": "Amassar as bananas."}]}
    page = f'<script type="application/ld+json">{json.dumps(data)}</script><ul><li>2 bananas</li><li>90g de aveia</li></ul>'
    monkeypatch.setattr(d.urllib.request, "urlopen", lambda req, timeout: Page(page.encode()))
    read = []

    def model(prompt, schema):
        if "lines" in schema["properties"]:
            return {"name": "Cookies", "category": "Sobremesas", "difficulty": "easy", "lines": [{"ingredients": []}]}
        if "unlisted" in schema["properties"]:
            return {"unlisted": ["bananas"]}
        read.append(prompt)
        return {"recipe": True, "name": "Cookies saudáveis", "category": "Sopas", "difficulty": "medium", "servings": 0, "time": 0,
                "ingredients": [], "steps": ["Amassar."]}
    monkeypatch.setattr(d, "ask_model", model)
    r = d.from_page("https://example.test/cookies")
    assert read[0].startswith("2 bananas\n90g de aveia") and "recipeIngredient" not in read[0]
    assert (r["name"], r["category"], r["difficulty"], r["steps"]) == ("Cookies", "Sobremesas", "easy", ["Amassar."])
    assert r["notes"] == ["the page's recipe data lacks bananas, which its steps use: drafted from the page's text"]


def test_check_flags_missing_fields():
    r = recipe(evidence="Caption:\npancetta", servings=None, steps=[], image=None)
    assert d.check(r, NAMES) == ["no servings in the source", "no steps in the source", "no photo in the source"]


def test_cooklang_parses(tmp_path):
    r = recipe(("cebola", "2", "", "picada (fina)", "2 cebolas"), ("sal", "", "", "", "sal"))
    text = d.cooklang(r, "Teste", ["sal: no quantity"])
    assert "@cebola{2}(picada fina), @sal{}." in text and 'image: "Teste.jpg"' in text and "tags:" not in text
    assert "---\n\n> Check: sal: no quantity\n\n@cebola" in text
    Path(tmp_path, "Teste.cook").write_text(text)
    subprocess.run([d.COOK, "doctor", "validate", "--strict", "-b", str(tmp_path)], check=True, capture_output=True)


def test_text_keeps_only_the_numbers_it_states(monkeypatch):
    monkeypatch.setattr(d, "ask_model", lambda prompt, schema: {
        "recipe": True, "name": "torradas", "category": "Pequeno-almoço", "difficulty": "easy", "servings": 2, "time": 15,
        "ingredients": [{"name": "pão", "quantity": "4", "unit": "fatias", "note": ""}], "steps": ["Torrar."]})
    r = d.from_text("Torradas, 2 porções, 10 minutos: 4 fatias de pão. Torrar.")
    assert (r["servings"], r["time"], r["notes"]) == (2, None, ["the source does not say time 15, left out"])


def test_new_takes_the_name_and_category_the_user_gave(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "")
    monkeypatch.setattr(d, "translate", lambda r, keep_name: False)
    monkeypatch.setattr(d, "from_text", lambda text: recipe(("ovo", "1", "", "", None), evidence=text, name="Pequeno-almoço",
                                                            category="Sobremesas", image=None, notes=[]))
    d.new(None, "1 ovo", "Ovo cozido", "Pequeno-almoço")
    assert (d.DRAFTS / "Pequeno-almoço" / "Ovo cozido.cook").exists()


def test_cooklang_quotes_what_yaml_would_misread(tmp_path):
    text = d.cooklang(recipe(("sal", "", "", "", "sal"), author="Receitas: rápidas"), "Teste")
    assert 'author: "Receitas: rápidas"' in text
    Path(tmp_path, "Teste.cook").write_text(text)
    subprocess.run([d.COOK, "doctor", "validate", "--strict", "-b", str(tmp_path)], check=True, capture_output=True)


def test_new_saves_no_photo_that_is_not_a_web_address(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "")
    monkeypatch.setattr(d, "translate", lambda r, keep_name: False)
    monkeypatch.setattr(d, "from_page", lambda url: recipe(("sal", "", "", "", "sal"), name="Bom", image="file:///etc/passwd", notes=[]))
    d.new("https://example.test/r", None)
    assert "the photo could not be saved: file:///etc/passwd" in (d.DRAFTS / "Pratos" / "Bom.cook").read_text()
    assert not (d.DRAFTS / "Pratos" / "Bom.jpg").exists()


def test_new_keeps_an_existing_draft(tmp_path, monkeypatch):
    monkeypatch.setattr(d, "DRAFTS", tmp_path)
    monkeypatch.setattr(d, "translate", lambda r, keep_name: False)
    monkeypatch.setattr(d, "from_page", lambda url: recipe(("sal", "", "", "", "sal"), name="Bom"))
    (tmp_path / "Sopas").mkdir()
    (tmp_path / "Sopas" / "bom.cook").write_text("edited")   # in any folder, in any case
    with pytest.raises(SystemExit):
        d.new("https://example.test/r", None)
    assert (tmp_path / "Sopas" / "bom.cook").read_text() == "edited" and not (tmp_path / "Pratos").exists()


def test_promote_refuses_a_draft_that_does_not_validate(tmp_path, monkeypatch):
    monkeypatch.setattr(d, "DRAFTS", tmp_path / "drafts")
    monkeypatch.setattr(d, "LIBRARY", tmp_path / "library")
    (d.DRAFTS / "Pratos").mkdir(parents=True)
    d.LIBRARY.mkdir()
    (d.DRAFTS / "Pratos" / "Mau.cook").write_text("---\nservings: [\n---\n\nMix.\n")
    with pytest.raises(SystemExit):
        d.promote("Mau")
    assert not any(d.LIBRARY.iterdir())


def test_promote_waits_for_every_check_to_be_deleted(tmp_path, monkeypatch):
    monkeypatch.setattr(d, "DRAFTS", tmp_path / "drafts")
    monkeypatch.setattr(d, "LIBRARY", tmp_path / "library")
    (d.DRAFTS / "Pratos").mkdir(parents=True)
    d.LIBRARY.mkdir()
    (d.DRAFTS / "Pratos" / "Bom.cook").write_text(d.cooklang(recipe(("sal", "", "", "", "sal")), "Bom", ["no photo in the source"]))
    with pytest.raises(SystemExit, match="1 open check"):
        d.promote("Bom")
    assert not any(d.LIBRARY.iterdir())


def test_promote_writes_what_formatting_asks_into_the_draft(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "")
    (d.DRAFTS / "Pratos").mkdir(parents=True)
    (d.DRAFTS / "Pratos" / "Bom.cook").write_text("---\nservings: 2\ndifficulty: easy\n---\n\n"
                                                  "@manteiga{20%gramas}, @manteiga{5%g}, @ovo{2%unidades}.\n\nMix.\n")
    (d.DRAFTS / "Pratos" / "Bom.jpg").write_bytes(b"jpg")
    with pytest.raises(SystemExit, match="4 fix"):
        d.promote("Bom")
    text = (d.DRAFTS / "Pratos" / "Bom.cook").read_text()
    assert text.startswith("---\nimage: Bom.jpg\nservings: 2\n") and "---\n\n> Check: no time\n" in text
    assert "> Check: manteiga: write 'gramas' as 'g'\n> Check: ovo: a plain count has no unit, not 'unidades'\n" in text
    assert not (d.LIBRARY / "Pratos").exists()


def test_promote_moves_draft_and_photo(tmp_path, monkeypatch):
    monkeypatch.setattr(d, "DRAFTS", tmp_path / "drafts")
    monkeypatch.setattr(d, "LIBRARY", tmp_path / "library")
    (d.DRAFTS / "Sopas").mkdir(parents=True)
    d.LIBRARY.mkdir()
    (d.DRAFTS / "Sopas" / "Bom.cook").write_text(d.cooklang(recipe(("sal", "", "", "", "sal")), "Bom"))
    (d.DRAFTS / "Sopas" / "Bom.jpg").write_bytes(b"jpg")
    d.promote("bom")
    assert sorted(p.relative_to(d.LIBRARY).as_posix() for p in d.LIBRARY.rglob("*.*")) == ["Sopas/Bom.cook", "Sopas/Bom.jpg"]
    assert not any((d.DRAFTS / "Sopas").iterdir())


def test_promote_refuses_a_draft_outside_a_category_or_a_name_taken_in_another(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "", **{"Sopas/Bom": "---\nservings: 2\ntime: 5\ndifficulty: easy\n---\n\n@sal{}."})
    (d.DRAFTS / "Outros").mkdir(parents=True)
    (d.DRAFTS / "Outros" / "Mau.cook").write_text(d.cooklang(recipe(("sal", "", "", "", "sal"), image=None), "Mau"))
    with pytest.raises(SystemExit, match="in no category"):
        d.promote("Mau")
    (d.DRAFTS / "Pratos").mkdir()
    (d.DRAFTS / "Pratos" / "Bom.cook").write_text(d.cooklang(recipe(("sal", "", "", "", "sal"), image=None), "Bom"))
    with pytest.raises(SystemExit, match="exists already"):
        d.promote("Bom")


def library(tmp_path, monkeypatch, conf, **recipes):
    monkeypatch.setattr(d, "DRAFTS", tmp_path / "drafts")
    monkeypatch.setattr(d, "LIBRARY", tmp_path / "library")
    (d.LIBRARY / "config").mkdir(parents=True)
    (d.LIBRARY / "config" / "aisle.conf").write_text(conf)
    for name, body in recipes.items():
        (d.LIBRARY / f"{name}.cook").parent.mkdir(exist_ok=True)
        (d.LIBRARY / f"{name}.cook").write_text(f"{body}\n\nMix.\n")


def test_missing_reads_cook_doctor(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "[laticínios]\nbutter\n", **{"Pratos/T": "@butter{1%g}, @pancetta{2%g}."})
    assert d.missing(d.LIBRARY) == {"pancetta": ["Pratos/T.cook"]}


def test_aisle_joins_adds_and_never_names_a_product_twice(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "[legumes]\ncebola|cebolas\n\n[mercearia]\nsal\n", **{"Pratos/T": "@onion{1}, @pancetta{2%g}, @sal{}."})
    monkeypatch.setattr(d, "ask_model", lambda prompt, schema: {"items": [
        {"name": "onion", "section": "legumes", "joins": "cebola", "names": ["onions", "cebola"]},
        {"name": "pancetta", "section": "mercearia", "joins": "", "names": ["sal"]}]})
    d.aisle()
    assert (d.DRAFTS / "aisle.conf").read_text() == "[legumes]\ncebola|cebolas|onion|onions\n\n[mercearia]\nsal\npancetta\n"
    d.promote("aisle.conf")
    assert (d.LIBRARY / "config" / "aisle.conf.bak").read_text() == "[legumes]\ncebola|cebolas\n\n[mercearia]\nsal\n"
    assert d.missing(d.LIBRARY) == {}


def test_promote_replaces_a_recipe_and_carries_its_photo_and_plan_links(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "", **{"Pratos/Garlic toast": "---\nservings: 2\ntime: 10\ndifficulty: easy\n---\n\n@alho{1}."})
    (d.LIBRARY / "Pratos" / "Garlic toast.jpg").write_bytes(b"jpg")
    (d.LIBRARY / "plans").mkdir()
    (d.LIBRARY / "plans" / "w.menu").write_text("- @./Pratos/Garlic toast{} \\\n- sobras: Garlic toast\n")
    dish = {"source": "library", "id": "Pratos/Garlic toast", "name": "Garlic toast",
            "url": "https://recipes.test/recipe/Pratos/Garlic%20toast.html"}
    monkeypatch.setattr(d, "PLAN", tmp_path / "plan.json")
    d.PLAN.write_text(json.dumps({"exclude": ["Pratos/Garlic toast"], "plan": {"days": [{"main": dish}], "soups": []}}))
    (d.DRAFTS / "Pequeno-almoço").mkdir(parents=True)
    (d.DRAFTS / "Pequeno-almoço" / "Torrada de alho.cook").write_text("---\nservings: 2\ntime: 10\ndifficulty: easy\n---\n\n@alho{1}.\n\nTorrar.\n")
    d.promote("Torrada de alho", "Garlic toast")
    assert sorted(p.relative_to(d.LIBRARY).as_posix() for p in d.LIBRARY.glob("*/*.*") if p.parent.name not in ("plans", "config")) == [
        "Pequeno-almoço/Torrada de alho.cook", "Pequeno-almoço/Torrada de alho.jpg"]
    moved = d.LIBRARY / "Pequeno-almoço" / "Torrada de alho.cook"
    assert (d.DRAFTS / "Garlic toast.cook.old").exists() and "image: Torrada de alho.jpg" in moved.read_text()
    assert (d.LIBRARY / "plans" / "w.menu").read_text() == "- @./Pequeno-almoço/Torrada de alho{} \\\n- sobras: Torrada de alho\n"
    assert json.loads(d.PLAN.read_text()) == {"exclude": ["Pequeno-almoço/Torrada de alho"], "plan": {"days": [{"main": {
        "source": "library", "id": "Pequeno-almoço/Torrada de alho", "name": "Torrada de alho",
        "url": "https://recipes.test/recipe/Pequeno-almo%C3%A7o/Torrada%20de%20alho.html"}}], "soups": []}}


def test_promote_keeps_the_replaced_photo_when_the_draft_has_its_own(tmp_path, monkeypatch):
    library(tmp_path, monkeypatch, "", **{"Pequeno-almoço/Garlic toast": "---\nservings: 2\ntime: 10\ndifficulty: easy\n---\n\n@alho{1}."})
    (d.LIBRARY / "Pequeno-almoço" / "Garlic toast.jpg").write_bytes(b"old")
    (d.DRAFTS / "Pequeno-almoço").mkdir(parents=True)
    (d.DRAFTS / "Pequeno-almoço" / "Torrada de alho.cook").write_text("---\nservings: 2\ntime: 10\ndifficulty: easy\n---\n\n@alho{1}.\n\nTorrar.\n")
    (d.DRAFTS / "Pequeno-almoço" / "Torrada de alho.jpg").write_bytes(b"new")
    d.promote("Torrada de alho", "Garlic toast")
    assert sorted(p.name for p in (d.LIBRARY / "Pequeno-almoço").iterdir()) == ["Torrada de alho.cook", "Torrada de alho.jpg"]
    assert (d.DRAFTS / "Garlic toast.jpg.old").read_bytes() == b"old"


def test_a_brazilian_word_is_a_check(tmp_path):
    path = tmp_path / "T.cook"
    path.write_text("---\nservings: 1\ntime: 5\ndifficulty: easy\n---\n\n@sal{}.\n\nUntar a assadeira e polvilhar com gergelim.\n")
    assert d.lint(path) == ["'assadeira' is Brazilian: write 'tabuleiro'", "'gergelim' is Brazilian: write 'sésamo'"]


def test_each_component_may_list_its_own_ingredients(tmp_path):
    path = tmp_path / "T.cook"
    path.write_text("---\nservings: 1\ntime: 5\ndifficulty: easy\n---\n\n= Massa =\n\n@sal{1%g}.\n\nMix.\n\n= Molho =\n\n@sal{2%g}.\n\nMix.\n")
    assert d.lint(path) == []
