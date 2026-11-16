# Run `nix run .#check-updates` to check for newer upstream releases.
_final: prev: let
  inherit (prev) lib;
  images = {
    cleanuparr = {
      image = "ghcr.io/cleanuparr/cleanuparr";
      version = "2.10.12";
      homepage = "https://github.com/Cleanuparr/Cleanuparr";
    };
    kapowarr = {
      image = "docker.io/mrcas/kapowarr";
      version = "1.3.2";
      tagPrefix = "v";
      homepage = "https://github.com/Casvt/Kapowarr";
    };
    livesync-cli = {
      image = "ghcr.io/vrtmrz/livesync-cli";
      version = "1.0.42";
      tagSuffix = "-cli";
      homepage = "https://github.com/vrtmrz/obsidian-livesync";
    };
    papra = {
      image = "ghcr.io/papra-hq/papra";
      version = "26.7.1";
      tagSuffix = "-rootless";
      homepage = "https://github.com/papra-hq/papra";
    };
    gufo = {
      image = "ghcr.io/gufo-org/toolboxes/gufo-runtime";
      version = "0.34.0";
      homepage = "https://github.com/gufo-org/gufo";
    };
  };
  tagged = lib.mapAttrs (_: img: let
    tagPrefix = img.tagPrefix or "";
    tagSuffix = img.tagSuffix or "";
  in img // { inherit tagPrefix tagSuffix; tag = "${tagPrefix}${img.version}${tagSuffix}"; }) images;
in {
  containerImages = tagged;
  # check-updates matches registry tags, so it needs the full tag shape.
  trackedContainerVersions = lib.mapAttrsToList (name: img: {
    inherit name;
    inherit (img) version image tagPrefix tagSuffix;
  } // lib.optionalAttrs (img ? versionPattern) {
    inherit (img) versionPattern;
  }) tagged;
}
