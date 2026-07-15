{ self, inputs, fleet, private, lib, ... }:
{
  imports = [ inputs.home-manager.nixosModules.home-manager ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    sharedModules = lib.attrValues self.homeManagerModules;
    extraSpecialArgs = { inherit self inputs fleet private; };  # mirror the host's specialArgs into HM
  };
}
