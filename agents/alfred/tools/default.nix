# Alfred's meals, cookidoo and drafts programs, tested on build.
{ pkgs, lib, facts }:
let
  inherit (facts) recipesRoot recipesUrl stateRoot cookidooCollection modelUrl model speechUrl;

  # Filled once for real and once against ./fixtures for the tests.
  fill = name: values:
    let
      text = builtins.replaceStrings (map (k: "@${k}@") (builtins.attrNames values)) (builtins.attrValues values)
        (builtins.readFile (./. + "/${name}/${name}.py"));
      left = builtins.filter builtins.isList (builtins.split "@([A-Za-z]+)@" text);
    in
    lib.throwIf (left != [ ]) "${name}.py: no value for ${lib.concatMapStringsSep ", " (m: "@${builtins.head m}@") left}" text;
  cook = lib.getExe pkgs.cook-cli;

  tests = pkgs.runCommand "alfred-tools-tests" {
    nativeBuildInputs = [ (pkgs.python3.withPackages (ps: with ps; [ cookidoo-api mcp recipe-scrapers pytest ])) ];
  } (let
    fixture = { tokenFile = "token.json"; planFile = "plan.json"; library = "${./fixtures}"; site = "https://recipes.test";
                collection = "test"; cookidoo = "cookidoo"; drafts = "drafts"; modelUrl = "http://model.test";
                model = "test"; speechUrl = "http://speech.test"; ytDlp = "yt-dlp"; curl = "curl"; magick = "magick";
                inherit cook; };
  in ''
    cp ${pkgs.writeText "cookidoo.py" (fill "cookidoo" fixture)} cookidoo.py
    cp ${pkgs.writeText "meals.py" (fill "meals" fixture)} meals.py
    cp ${pkgs.writeText "drafts.py" (fill "drafts" fixture)} drafts.py
    cp ${./cookidoo/test_cookidoo.py} test_cookidoo.py
    cp ${./meals/test_meals.py} test_meals.py
    cp ${./drafts/test_drafts.py} test_drafts.py
    python -m pytest -q -p no:cacheprovider
    touch $out
  '');

  # Depending on the tests means a failing program never builds, so it never deploys.
  program = name: libraries: values: (pkgs.writers.writePython3Bin name { inherit libraries; flakeIgnore = [ "E501" ]; }
    (fill name values)).overrideAttrs { inherit tests; };

  # On the state volume, writable: every refresh rotates the token.
  cookidoo = program "cookidoo" [ pkgs.python3Packages.cookidoo-api ] {
    tokenFile = "${stateRoot}/cookidoo-token.json"; library = "${recipesRoot}/library"; collection = cookidooCollection;
    inherit cook;
  };
  meals = program "meals" [ pkgs.python3Packages.mcp ] {
    planFile = "${stateRoot}/meals-plan.json"; library = "${recipesRoot}/library"; site = recipesUrl;
    cookidoo = lib.getExe cookidoo; inherit cook;
  };
  # Drafts sit beside the library, so the site, plans and shopping lists never see them.
  drafts = program "drafts" [ pkgs.python3Packages.recipe-scrapers ] {
    library = "${recipesRoot}/library"; drafts = "${recipesRoot}/drafts"; inherit modelUrl model speechUrl cook;
    ytDlp = lib.getExe pkgs.yt-dlp; curl = lib.getExe pkgs.curl; magick = lib.getExe' pkgs.imagemagick "magick";
  };
in
{
  inherit cookidoo meals drafts;
}
