final: prev: {
  proton-ge-custom = final.callPackage ./proton-ge-custom {};
  preview = final.callPackage ./preview {};
}

# Example on how to bring unstable within scope https://github.com/ethanabrooks/nix/blob/main/overlays/default.nix#L18
