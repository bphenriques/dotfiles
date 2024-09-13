{ inputs, lib }: {
  hosts = import ./hosts.nix { inherit inputs lib; };
  builders = import ./builders.nix { inherit inputs lib; };
}