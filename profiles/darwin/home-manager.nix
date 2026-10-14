{ self, inputs, fleet, lib, ... }:
{
  imports = [ inputs.home-manager.darwinModules.home-manager ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    sharedModules = lib.attrValues self.homeManagerModules;
    extraSpecialArgs = { inherit self inputs fleet; };  # mirror the host's specialArgs into HM
  };
}
