{ inputs }:
inputs.nixpkgs.lib.extend(self: _:
  let lib = self; in {
    my = {
      hosts = import ./hosts.nix { inherit lib inputs; };
    };
  }
)