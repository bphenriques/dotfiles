_: {
  git-versions = (final: prev: {
    # Remove on next release when it supports "--with-nth"
    fuzzel-git = prev.fuzzel.overrideAttrs (_: {
      version = "git";
      src = prev.fetchFromGitea {
        domain = "codeberg.org";
        owner = "dnkl";
        repo = "fuzzel";
        rev = "9a0a38de3703747c183bdd441e5d694c3ef895aa";
        hash = "sha256-4AJW8lTJuN6MPvFlFzmM1DMsx72WSi93FUYiVPv/rwU=";
      };
    });

    # I could overwrite the version but the build system is confusing to mess in this case. It is simpler to copy.
    wlr-which-key-git = prev.callPackage ./wlr-which-key-git.nix { };
  });
}
