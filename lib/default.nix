{ inputs, lib }: {
  hosts = import ./hosts.nix { inherit inputs lib; };
  builders = import ./builders.nix { inherit lib; };
  generators = import ./generators.nix { inherit lib; };
}