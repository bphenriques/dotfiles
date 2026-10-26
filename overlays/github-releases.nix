_final: prev: let
  inherit (prev) lib;
  packages = {
    elegantfin-jellyfin-theme = prev.stdenvNoCC.mkDerivation rec {
      pname = "elegantfin-jellyfin-theme";
      version = "26.09.05";

      src = prev.fetchurl {
        url = "https://cdn.jsdelivr.net/gh/lscambo13/ElegantFin@v${version}/Theme/ElegantFin-jellyfin-theme-build-latest-minified.css";
        hash = "sha256-d5qoAbkS0hCJ1Ijd9aQm/IwClw7JU1Zs7uQW4Ni85kM=";
      };

      dontUnpack = true;
      phases = [ "installPhase" ];
      installPhase = "install -Dm444 $src $out";

      passthru.updateInfo = { repo = "lscambo13/ElegantFin"; stripPrefix = "v"; };
      meta = {
        description = "ElegantFin CSS theme for Jellyfin";
        homepage = "https://github.com/lscambo13/ElegantFin";
        platforms = lib.platforms.all;
      };
    };

    # The Cooklang MCP server; the checksum is the one its npm installer verifies.
    cook-mcp = prev.stdenv.mkDerivation rec {
      pname = "cook-mcp";
      version = "0.2.3";

      src = prev.fetchurl {
        url = "https://github.com/cook-md/cook-mcp/releases/download/v${version}/cook-mcp-x86_64-unknown-linux-gnu.tar.gz";
        sha256 = "fa7e9425b1cddcbda8bb09f55d1371d15a340d9513e44d194ac89e4c1dea2378";
      };

      sourceRoot = ".";
      nativeBuildInputs = [ prev.autoPatchelfHook ];
      buildInputs = [ prev.stdenv.cc.cc.lib ];
      installPhase = "install -Dm755 cook-mcp $out/bin/cook-mcp";

      passthru.updateInfo = { repo = "cook-md/cook-mcp"; stripPrefix = "v"; };
      meta = {
        description = "MCP server for Cooklang recipes";
        homepage = "https://github.com/cook-md/cook-mcp";
        platforms = [ "x86_64-linux" ];
        mainProgram = "cook-mcp";
      };
    };
  };
in
  packages // {
    trackedGithubVersions = lib.mapAttrsToList (_: pkg: {
      name = pkg.pname;
      inherit (pkg) version;
      inherit (pkg.passthru.updateInfo) repo stripPrefix;
    }) (lib.filterAttrs (_: pkg: pkg ? passthru && pkg.passthru ? updateInfo) packages);
  }
