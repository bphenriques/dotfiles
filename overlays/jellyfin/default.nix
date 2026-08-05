_final: prev: let
  inherit (prev) lib;
  packages = {
    elegantfin-jellyfin-theme = prev.stdenvNoCC.mkDerivation rec {
      pname = "elegantfin-jellyfin-theme";
      version = "26.06.06";

      src = prev.fetchurl {
        url = "https://cdn.jsdelivr.net/gh/lscambo13/ElegantFin@v${version}/Theme/ElegantFin-jellyfin-theme-build-latest-minified.css";
        hash = "sha256-XCzoCb1Ylj785EF1XYixCb8Tn09ZT8YBsdarj/t/cK4=";
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
