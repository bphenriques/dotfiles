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
  };
in
  packages // {
    trackedGithubVersions = lib.mapAttrsToList (_: pkg: {
      name = pkg.pname;
      inherit (pkg) version;
      inherit (pkg.passthru.updateInfo) repo stripPrefix;
    }) (lib.filterAttrs (_: pkg: pkg ? passthru && pkg.passthru ? updateInfo) packages);
  }
