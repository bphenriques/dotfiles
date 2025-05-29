_: {
  git-versions = (final: prev: {
    # Remove on next release when the niri fix is available
    wl-kbptr-git = prev.wl-kbptr.overrideAttrs (_: {
      version = "git";
      src = prev.fetchFromGitHub {
        owner = "moverest";
        repo = "wl-kbptr";
        rev = "1db88d904f124636c7ade8fdd7881d0fd2d6caf1";
        hash = "sha256-A/PS5H5MuYNOA9uUhqUU+Z4PfXoAwmyQQXXYF0VfTNM=";
     };
    });

    # I could overwrite the version but the build system is confusing to mess in this case. It is simpler to copy.
    wlr-which-key-git = final.callPackage ./wlr-which-key-git.nix { };
    igir-git = final.callPackage ./igir-git.nix { };
  });
}
