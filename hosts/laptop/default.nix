{ inputs, mylib }:
let
  inherit (inputs.nixpkgs.lib.attrsets) attrValues;
  system = "x86_64-linux";
  # Ideally modules are imported in the file that uses it. However, it leads to a infinite recursion. A rabbit-hole to debug.
  overlays = attrValues inputs.self.overlays ++ [ inputs.nur.overlay ];
  nixosModules = attrValues inputs.self.nixosModules ++ [
    inputs.sops-nix.nixosModules.sops
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
  ];
  hmModules = attrValues inputs.self.homeManagerModules;
  specialArgs = {
    self = {
      pkgs = inputs.self.packages.${system} // inputs.dotfiles-private.packages.${system};
      lib = mylib;
      themes = import ../../themes { lib = inputs.nixpkgs.lib; };
    };

    community.pkgs = {
      firefox-addons = inputs.firefox-addons.packages.${system};
    };
  };
in mylib.hosts.mkNixOSHost {
  inherit system nixosModules hmModules overlays;
  nixosSpecialArgs = specialArgs;
  hmSpecialArgs    = specialArgs;
  hostModule = ./config.nix;
}
