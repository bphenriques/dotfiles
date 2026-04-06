_: {
  git-versions = (_final: _prev: {
    # intentionally left empty
  });

  # Run `nix run .#check-updates` to check for newer upstream releases.
  pinned-github-releases = (_final: prev: let
    lib = prev.lib;
    packages = {
      elegantfin-jellyfin-theme = prev.stdenvNoCC.mkDerivation rec {
        pname = "elegantfin-jellyfin-theme";
        version = "25.12.31";

        src = prev.fetchurl {
          url = "https://cdn.jsdelivr.net/gh/lscambo13/ElegantFin@v${version}/Theme/ElegantFin-jellyfin-theme-build-latest-minified.css";
          hash = "sha256-utg6R5Qbmm/7tckWqMCh33wLAORG+4QROqXxmzmjL1U=";
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

      jellyfin-plugin-sso = prev.stdenvNoCC.mkDerivation rec {
        pname = "jellyfin-plugin-sso";
        version = "4.0.0.3";

        src = prev.fetchzip {
          url = "https://github.com/9p4/jellyfin-plugin-sso/releases/download/v${version}/sso-authentication_${version}.zip";
          hash = "sha256-Jkuc+Ua7934iSutf/zTY1phTxaltUkfiujOkCi7BW8w=";
          stripRoot = false;
        };

        dontUnpack = true;
        phases = [ "installPhase" ];
        installPhase = "cp -r $src/. $out/";

        passthru.jellyfin.pluginDirName = "SSO-Auth";
        passthru.updateInfo = { repo = "9p4/jellyfin-plugin-sso"; stripPrefix = "v"; };
        meta = {
          description = "Jellyfin SSO authentication plugin";
          homepage = "https://github.com/9p4/jellyfin-plugin-sso";
          platforms = lib.platforms.all;
        };
      };

      jellyfin-plugin-open-subtitles = prev.stdenvNoCC.mkDerivation rec {
        pname = "jellyfin-plugin-open-subtitles";
        version = "23";

        src = prev.fetchzip {
          url = "https://repo.jellyfin.org/releases/plugin/open-subtitles/open-subtitles_${version}.0.0.0.zip";
          hash = "sha256-+5gwpkZliE5Kb3JKqcUDAAZDZ0UXNue4NkUgdn0fYMA=";
          stripRoot = false;
        };

        dontUnpack = true;
        phases = [ "installPhase" ];
        installPhase = "cp -r $src/. $out/";

        passthru.jellyfin.pluginDirName = "Open Subtitles";
        passthru.updateInfo = { repo = "jellyfin/jellyfin-plugin-opensubtitles"; stripPrefix = "v"; };
        meta = {
          description = "Jellyfin Open Subtitles plugin";
          homepage = "https://github.com/jellyfin/jellyfin-plugin-opensubtitles";
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
  );

  # Run `nix run .#check-updates` to check for newer upstream releases.
  pinned-container-images = (_final: prev: let
    lib = prev.lib;
    images = {
      grist = {
        image = "gristlabs/grist";
        version = "1.7.12";
        updateInfo = { repo = "gristlabs/grist-core"; stripPrefix = "v"; };
      };
      larapaper = {
        image = "ghcr.io/usetrmnl/larapaper";
        version = "0.31.3";
        updateInfo = { repo = "usetrmnl/larapaper"; };
      };
      romm = {
        image = "rommapp/romm";
        version = "4.8.0";
        updateInfo = { repo = "rommapp/romm"; };
      };
    };
  in {
    containerImages = images;
    trackedContainerVersions = lib.mapAttrsToList (name: img: {
      inherit name;
      inherit (img) version;
      repo = img.updateInfo.repo;
      stripPrefix = img.updateInfo.stripPrefix or "";
    }) images;
  });
}
