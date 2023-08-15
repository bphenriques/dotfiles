final: prev: {
  frg = final.callPackage ./frg {};
  preview = final.callPackage ./preview {};
  proton-ge-custom = final.callPackage ./proton-ge-custom {};
}

# TODO: fold over the directories and use final.callPackage. Do it after adding pkgsUnstable within scope.
# Example on how to bring unstable within scope https://github.com/ethanabrooks/nix/blob/main/overlays/default.nix#L18
