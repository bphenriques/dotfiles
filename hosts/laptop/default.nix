{ self, mylib, nixpkgs, home-manager, sops-nix, disko, nur, ... }:
let
  inherit (nixpkgs.lib.attrsets) attrValues;

  overlays = attrValues self.overlays ++ [ nur.overlay ];
  nixosModules = attrValues self.nixosModules ++ [
    sops-nix.nixosModules.sops
    disko.nixosModules.disko
    home-manager.nixosModules.home-manager
  ];
  hmModules = attrValues self.homeManagerModules;
in mylib.hosts.mkNixOSHost {
  inherit nixosModules hmModules overlays;
  extraSpecialArgs = {
    host.hardware.webcam = "/dev/video0";
    servers = import ../servers.nix;
  };
  hostModule = ./config.nix;
}
