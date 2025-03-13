{ inputs, mylib }:
let
  inherit (inputs.nixpkgs.lib.attrsets) attrValues;

  system = "x86_64-linux";
  overlays = attrValues inputs.self.overlays ++ [ inputs.nur.overlays.default ];
  nixosModules = attrValues inputs.self.nixosModules ++ [
    inputs.sops-nix.nixosModules.sops
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
  ];
  hmModules = attrValues inputs.self.homeManagerModules ++ [
    inputs.stylix.homeManagerModules.stylix
  ];
  specialArgs = {
    self = {
      pkgs = inputs.self.packages.${system} // inputs.dotfiles-private.packages.${system};
      lib = mylib;
    };

    community.pkgs = {
      firefox-addons = inputs.firefox-addons.packages.${system};
    };
  };
in mylib.hosts.mkNixOSHost {
  inherit system nixosModules hmModules overlays;
  nixosSpecialArgs  = specialArgs;
  hmSpecialArgs     = specialArgs;
  hostModule        = ./config.nix;
}
