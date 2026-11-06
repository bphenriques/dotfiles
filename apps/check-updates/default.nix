{ lib, pkgs }:
let
  numeric = "[0-9][0-9.]*";   # plain releases only, so rc and variant tags drop out

  # Registry tags, not upstream releases: a release without a pushed image is not an update yet.
  containerEntry = img:
    let
      parts = lib.splitString "/" img.image;
      registry = lib.head parts;
      prefix = lib.escapeRegex img.tagPrefix;
      suffix = lib.escapeRegex img.tagSuffix;
    in {
      source = "container";
      container = lib.concatStringsSep "/" (lib.tail parts);
      include_regex = "${prefix}${img.versionPattern or numeric}${suffix}";
    } // lib.optionalAttrs (registry != "docker.io") { inherit registry; }
      // lib.optionalAttrs (prefix != "" || suffix != "") {
        from_pattern = "^${prefix}(.*)${suffix}$";
        to_pattern = "\\1";
      };

  githubEntry = pkg: {
    source = "github";
    github = pkg.repo;
    use_max_release = true;
    include_regex = "${lib.escapeRegex pkg.stripPrefix}${numeric}";
    prefix = pkg.stripPrefix;
  };

  tracked =
    map (pkg: pkg // { entry = githubEntry pkg; }) pkgs.trackedGithubVersions
    ++ map (img: img // { entry = containerEntry img; }) pkgs.trackedContainerVersions;

  config = (pkgs.formats.toml { }).generate "nvchecker.toml" ({
    __config__ = { oldver = "oldver.json"; newver = "newver.json"; };
  } // lib.listToAttrs (map (t: lib.nameValuePair t.name t.entry) tracked));

  oldver = pkgs.writeText "oldver.json" (builtins.toJSON {
    version = 2;
    data = lib.listToAttrs (map (t: lib.nameValuePair t.name { inherit (t) version; }) tracked);
  });
in
pkgs.writeShellApplication {
  name = "check-updates";
  runtimeInputs = [ pkgs.nvchecker ];
  runtimeEnv = {
    NVCHECKER_CONFIG = config;
    NVCHECKER_OLDVER = oldver;
  };
  text = lib.fileContents ./script.sh;
  meta.description = "Check for updates on pinned packages and container images";
}
