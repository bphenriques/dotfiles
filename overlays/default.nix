_: {
  git-versions = (_final: _prev: {
    # intentionally left empty
  });

  # Run `nix run .#check-updates` to check for newer upstream releases.
  pinned-github-releases = (_final: prev: let
    lib = prev.lib;

    # Helper for Jellyfin plugins distributed as zip archives (directory layout expected by Jellyfin).
    mkJellyfinPlugin =
      {
        pname,
        version,
        src,
        pluginDirName,
        updateInfo,
        description,
        homepage,
      }:
      prev.stdenvNoCC.mkDerivation {
        inherit pname version src;
        dontUnpack = true;
        phases = [ "installPhase" ];
        installPhase = "cp -r $src/. $out/";
        passthru = {
          jellyfin.pluginDirName = pluginDirName;
          inherit updateInfo;
        };
        meta = {
          inherit description homepage;
          platforms = lib.platforms.all;
        };
      };

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

      jellyfin-plugin-sso = mkJellyfinPlugin rec {
        pname = "jellyfin-plugin-sso";
        version = "4.0.0.3";
        src = prev.fetchzip {
          url = "https://github.com/9p4/jellyfin-plugin-sso/releases/download/v${version}/sso-authentication_${version}.zip";
          hash = "sha256-Jkuc+Ua7934iSutf/zTY1phTxaltUkfiujOkCi7BW8w=";
          stripRoot = false;
        };
        pluginDirName = "SSO-Auth";
        updateInfo = { repo = "9p4/jellyfin-plugin-sso"; stripPrefix = "v"; };
        description = "Jellyfin SSO authentication plugin";
        homepage = "https://github.com/9p4/jellyfin-plugin-sso";
      };

      jellyfin-plugin-open-subtitles = mkJellyfinPlugin rec {
        pname = "jellyfin-plugin-open-subtitles";
        version = "23";
        src = prev.fetchzip {
          url = "https://repo.jellyfin.org/releases/plugin/open-subtitles/open-subtitles_${version}.0.0.0.zip";
          hash = "sha256-+5gwpkZliE5Kb3JKqcUDAAZDZ0UXNue4NkUgdn0fYMA=";
          stripRoot = false;
        };
        pluginDirName = "Open Subtitles";
        updateInfo = { repo = "jellyfin/jellyfin-plugin-opensubtitles"; stripPrefix = "v"; };
        description = "Jellyfin Open Subtitles plugin";
        homepage = "https://github.com/jellyfin/jellyfin-plugin-opensubtitles";
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
}
