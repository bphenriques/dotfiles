{ inputs, mylib }:
let
  inherit (inputs.nixpkgs.lib.attrsets) attrValues;

  system = "aarch64-darwin";
  darwinModules = attrValues inputs.self.darwinModules ++ [ inputs.home-manager.darwinModules.home-manager ];
  hmModules = attrValues inputs.self.homeManagerModules;
  specialArgs = {
    self = {
      pkgs = inputs.self.packages.${system} // inputs.dotfiles-private.packages.${system};
      lib = mylib;
      themes = import ../../themes { lib = inputs.nixpkgs.lib; };
    };
  };
in mylib.hosts.mkMacOSHost {
  inherit system darwinModules hmModules;
  hostModule = ./config.nix;
  darwinSpecialArgs = specialArgs;
  hmSpecialArgs    = specialArgs;
}
